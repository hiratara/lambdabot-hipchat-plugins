{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lambdabot.Plugin.HipChat (hipChatPlugin, hipChatPlugins) where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Exception.Lifted
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.Foldable (forM_)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.XML.Types
import Data.Text (Text, pack, unpack)
import Lambdabot.Command
import Lambdabot.IRC
import Lambdabot.Module
import Lambdabot.Monad hiding (send)
import Lambdabot.State
import Network (PortID(..))
import Network.HTTP.Conduit
import Network.Xmpp hiding (to, message, from, jid)
import System.Timeout.Lifted (timeout)

data HipConfig = HipConfig
  { apiToken :: String
  , xmppUser :: String
  , xmppNick :: String
  , xmppPass :: String
  }

type RoomId = String
type RoomJid = String

data HipRoom = HipRoom
  { apiRoom :: RoomId
  , xmppRoom :: RoomJid
  }

data HipState = HipState
  { hipConf :: HipConfig
  , hipSession :: Session
  , hipRooms :: [HipRoom]
  , ignored :: [String]
  }

type S = Maybe (TVar HipState)

ircNick :: HipConfig -> String
ircNick = xmppNick

hipchatServer :: HipConfig -> String
hipchatServer = ("chat.hipchat.com/" ++) . xmppUser

hipChatPlugin :: Module S
hipChatPlugin = newModule
  { moduleCmds = return
    [ (command "hipchat-connect")
        { aliases = []
        , help = say "hipchat-connect <token> <xmpp_user> <xmpp_nick> <xmpp_pass>. Connect to HipChat."
        , process = connect
        }
    , (command "hipchat-join")
        { aliases = []
        , help = say "hipchat-join <room_id> <xmpp_room>. Join a HipChat room."
        , process = joinRoom
        }
    , (command "hipchat-ignore")
        { aliases = []
        , help = say "hipchat-ignore <xmpp_nick>+. Ignore messages from user."
        , process = addIgnore
        }
    ]
  , moduleDefState = return Nothing
  }

connect :: String -> Cmd (ModuleT S LB) ()
connect xs = case parseArgs xs of
  (authToken:jUser:jNick:jPass:_) -> do
    let hipconf = HipConfig authToken jUser jNick jPass
    v <- liftIO $ hipInit hipconf
    lift . lift . void . fork $ listenLoop v
    lift $ addServer (hipchatServer hipconf) (liftIO . sendHipMessage)
    writeMS $ Just v
  _ -> say "Not enough parameters!"

joinRoom :: String -> Cmd (ModuleT S LB) ()
joinRoom xs = case parseArgs xs of
  (roomId:roomName:_) ->
    readMS >>= \case
      Just v -> liftIO $ do
        readTVarIO v >>= \hs -> sendMUCPresence (hipConf hs) (hipSession hs) roomName
        atomically $ modifyTVar v $ \hs -> hs { hipRooms = HipRoom roomId roomName : hipRooms hs }
      Nothing ->
        say "Not connected to HipChat. Use hipchat-connect before trying to join a room."
  _ -> say "Not enough parameters!"

addIgnore :: String -> Cmd (ModuleT S LB) ()
addIgnore xs = case parseArgs xs of
  [] -> say "Not enough parameters!"
  ns  -> readMS >>= \case
      Just v -> liftIO $ atomically $
        modifyTVar v $ \hs -> hs { ignored = ignored hs <> ns }
      Nothing ->
        say "Not connected to HipChat. Use hipchat-connect before trying to ignore users."

parseArgs :: String -> [String]
parseArgs = skipSpaces
  where
    parseArgs' [] = []
    parseArgs' ('"':xs) = let (arg, _:left) = break (== '"') xs
                          in arg : skipSpaces left
    parseArgs' xs = let (arg, left) = break (== ' ') xs
                          in arg : skipSpaces left
    skipSpaces xs = let (_, left) = break (/= ' ') xs
                    in parseArgs' left

hipChatPlugins :: [String]
hipChatPlugins = ["hipChat"]

data HipMessage
  = TextMessage Text
  | HtmlMessage Text

msgText :: HipMessage -> Text
msgText m = case m of
  TextMessage t -> t
  HtmlMessage t -> t

msgFormat :: HipMessage -> Text
msgFormat m = case m of
  TextMessage _ -> "text"
  HtmlMessage _ -> "html"

instance ToJSON HipMessage where
  toJSON m = object
    [ "message" .= msgText m
    , "message_format" .= msgFormat m
    ]

hipMessage :: IrcMessage -> HipMessage
hipMessage ircMsg = case ircMsgParams ircMsg of
  _:_:html:_ -> HtmlMessage $ pack html
  _:text:[] -> TextMessage $ pack $ tail text
  _ -> error "No message"

sendHipMessage :: IrcMessage -> IO ()
sendHipMessage ircMsg = initRq >>= send where
  initRq = parseUrl $ head $ ircMsgParams ircMsg
  send req' = void $ withManager $ httpLbs (req'
    { method = "POST"
    , requestBody = RequestBodyLBS (encode $ hipMessage ircMsg)
    , requestHeaders = (header ("Content-Type", "application/json") . requestHeaders) req'
    , checkStatus = \_ _ _ -> Nothing
    })
  header (h, k) hdrs =
    let hdrs' = filter ((/= h) . fst) hdrs
    in (h, k) : hdrs'

roomSendUrl :: HipState -> RoomJid -> String
roomSendUrl hs room = concat
  [ "https://api.hipchat.com/v2/room/"
  , fromMaybe room (lookupRoomId hs room)
  , "/notification?auth_token="
  , apiToken $ hipConf hs
  ]

lookupRoomId :: HipState -> RoomJid -> Maybe RoomId
lookupRoomId hs n = apiRoom <$> find (\r -> xmppRoom r == n) (hipRooms hs)

userSendUrl :: HipState -> String -> String
userSendUrl hs to = concat
  [ "https://api.hipchat.com/v2/user/"
  , drop 1 $ dropWhile (/= '_') to
  , "/message?auth_token="
  , apiToken $ hipConf hs
  ]

listenLoop :: TVar HipState -> LB ()
listenLoop v = forever $ catch loop' handleErr where
  loop' = do
    mes <- wait
    hs <- liftIO $ readTVarIO v
    unless (ignoreMsg mes hs) (rcv mes hs)
  wait = liftIO $ readTVarIO v >>= getMessage . hipSession
  ignoreMsg mes hs
    | not $ null $ elems "delay" mes = True
    | null $ elems "body" mes = True
    | otherwise = case messageType mes of
        Chat -> False
        _ -> maybe False ((`elem` ignored hs) . unpack) (messageFrom mes >>= resourcepart)
  handleErr :: SomeException -> LB ()
  handleErr = liftIO . print

rcv :: Message -> HipState -> LB ()
rcv mes hs = forM_ channel rcv' where
  rcv' = void . fork . void . timeout 15000000 . received . ircMsg
  ircMsg ch =
    let body = head $ elementText (head $ elems "body" mes)
        srv = hipchatServer $ hipConf hs
    in IrcMessage
      { ircMsgServer = srv
      , ircMsgLBName = ircNick $ hipConf hs
      , ircMsgPrefix = from
      , ircMsgCommand = "PRIVMSG"
      , ircMsgParams = [ concat [ srv, ":", ch ], ':' : unpack body]
      }
  from = maybe "(anybody)" unpack (part =<< messageFrom mes) where
    part = case messageType mes of
      Chat -> localpart
      _ -> resourcepart
  channel = case messageType mes of
    Chat -> userSendUrl hs . unpack <$> (localpart =<< messageFrom mes)
    _    -> roomSendUrl hs . unpack <$> groupChannel where
      groupChannel = do
        jid <- messageFrom mes
        lp <- localpart jid
        return $ mconcat [ lp, "@", domainpart jid ]

elems :: Text -> Message -> [Element]
elems tagname mes = filter byTag $ messagePayload mes where
  byTag = (== tagname) . nameLocalName . elementName

hipInit :: HipConfig -> IO (TVar HipState)
hipInit hipconf = do
  let stconf = def { connectionDetails = UseHost "chat.hipchat.com" (PortNumber 5222) }
      user' = pack $ xmppUser hipconf
      pass' = pack $ xmppPass hipconf
  result <- session
    "chat.hipchat.com"
    (Just (const [plain user' Nothing pass'], Nothing))
    def { sessionStreamConfiguration = stconf } --, enableRoster = False }
  sess <- case result of
    Left e -> error $ "XmppFailure: " ++ show e
    Right s -> return s
  v <- liftIO $ newTVarIO $ HipState hipconf sess [] []
  liftIO $ setConnectionClosedHandler (\_ _ -> reconn v) sess
  return v

reconn :: TVar HipState -> IO ()
reconn v = void $ readTVarIO v >>= \hs -> do
  let conf = hipConf hs
      sess = hipSession hs
  _ <- reconnectNow sess
  forM_ (hipRooms hs) (sendMUCPresence conf sess . xmppRoom)

sendMUCPresence :: HipConfig -> Session -> String -> IO ()
sendMUCPresence hipconf sess room = void $ getJid sess >>= send where
    send = flip sendPresence sess . pres
    pres jid = def
      { presenceFrom = jid
      , presenceTo = Just . parseJid $ room ++ '/' : xmppNick hipconf
      , presencePayload = [Element "x" [(Name "xmlns" Nothing Nothing, [ContentText "http://jabber.org/protocol/muc"])] []]
      }
