{-# LANGUAGE OverloadedStrings #-}
module Lambdabot.Plugin.HipChat (hipChatPlugin, hipChatPlugins) where

import Lambdabot.Command (Cmd)
import Lambdabot.IRC (IrcMessage(..))
import Lambdabot.Module (ModuleT)
import Lambdabot.Monad (LB, received, addServer)
import Lambdabot.Plugin (
  Module(..), newModule, moduleCmds
  , aliases, help, process, say, command
  )
import Control.Concurrent.Lifted (fork)
import Control.Exception.Lifted (SomeException, catch)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (encode, object)
import Data.Aeson.Types (Value(String))
import Data.Text (Text, pack, unpack)
import Network.HTTP.Conduit (
  parseUrl, Request(..), RequestBody(RequestBodyLBS), withManager, httpLbs
  )
import Control.Monad
import Data.Default (def)
import Network.Xmpp (
  SessionConfiguration(sessionStreamConfiguration)
  , parseJid, getJid, resourcepart, Session, session, plain
  , Presence(presenceFrom, presenceTo, presencePayload)
  , sendPresence, getMessage, messageFrom, messagePayload
  )
import Data.XML.Types (
  nameLocalName, elementName, elementText
  , Element(Element), Name(Name), Content(ContentText)
  )
import System.Timeout.Lifted (timeout)

data HipConfig = HipConfig {
  apiToken :: String
  , apiRoom :: String
  , xmppUser :: String
  , xmppNick :: String
  , xmppPass :: String
  , xmppRoom :: String
  }

hipChatPlugin :: Module ()
hipChatPlugin = newModule
    { moduleCmds = return
        [ (command "hipchat")
            { aliases = []
            , help = say "hipchat <token> <room_id> <xmpp_user> <xmpp_nick> <xmpp_pass> <xmpp_room>. Connect to HipChat."
            , process = hipCommand
            }
        ]
    }
    where
      hipCommand :: String -> Cmd (ModuleT () LB) ()
      hipCommand xs = do
        let hipconf = let (authToken:room:jUser:jNick:jPass:jRoom:_) = parseArgs xs
                      in HipConfig authToken room jUser jNick jPass jRoom
        lift . lift . fork $ listenLoop hipconf
        lift $ addServer (xmppRoom hipconf) (sendMessage' hipconf)
        say ("Hello hip. " ++ xs)
      sendMessage' :: HipConfig -> IrcMessage -> LB ()
      sendMessage' hipconf ircmsg = do
        let msgs = ircMsgParams ircmsg
        liftIO $ sendHipMessage hipconf (pack . tail . head . tail $ msgs)

parseArgs :: String -> [String]
parseArgs xs' = skipSpaces xs'
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

sendHipMessageJSON :: Text -> ByteString
sendHipMessageJSON m = encode . object $ [
  ("message", String m), ("message_format", "text")
  ]

sendHipMessage :: HipConfig -> Text -> IO ()
sendHipMessage hipconf message = do
  let room  = apiRoom hipconf
      token = apiToken hipconf
  req' <- parseUrl $ "https://api.hipchat.com/v2/room/" ++ room ++
                     "/notification?auth_token=" ++ token
  let req = req' {
        method = "POST", requestBody = RequestBodyLBS (sendHipMessageJSON message)
        , requestHeaders = (
          header ("Content-Type", "application/json") . requestHeaders
          ) req'
        , checkStatus = \_ _ _ -> Nothing
        }
  _ <- withManager $ httpLbs req
  return ()
  where
    header (h, k) hs = let hs' = filter ((/= h) . fst) hs
                     in (h, k) : hs'

listenLoop :: HipConfig -> LB ()
listenLoop hipconf = do
  sess <- liftIO $ xmppListen hipconf
  forever $ catch (loop' sess) handleErr
  where
    loop' :: Session -> LB ()
    loop' sess = do
      mes <- liftIO $ getMessage sess
      let from = maybe "(anybody)" unpack (resourcepart =<< messageFrom mes)
      let bodyElems = elems "body" mes
      let delayElems = elems "delay" mes
      when (null delayElems && (not . null) bodyElems) $ do
        let body = head $ elementText (head bodyElems)
            room = xmppRoom hipconf
        void . fork . void . timeout 15000000 . received $ IrcMessage {
          ircMsgServer = room
          , ircMsgLBName = "lambdabottest"
          , ircMsgPrefix = from
          , ircMsgCommand = "PRIVMSG"
          , ircMsgParams = [room, ':' : unpack body]
          }
      return ()
    handleErr :: SomeException -> LB ()
    handleErr = liftIO . print
    elems tagname mes = filter ((== tagname) . nameLocalName . elementName) $
                               (messagePayload mes)

xmppListen :: HipConfig -> IO Session
xmppListen hipconf = do
    let stconf = def
    result <- session
                 "chat.hipchat.com"
                  (Just (\_ -> [plain (pack . xmppUser $ hipconf) Nothing (pack . xmppPass $ hipconf)]
                               , Nothing))
                  def { sessionStreamConfiguration = stconf }
    sess <- case result of
                Right s -> return s
                Left e -> error $ "XmppFailure: " ++ (show e)
    jid <- getJid $ sess
--    _ <- sendPresence def sess -- Send broad <presence>
    _ <- sendPresence (def {
           presenceFrom = jid
           , presenceTo = Just . parseJid $ (xmppRoom hipconf) ++ '/' : (xmppNick hipconf)
           , presencePayload = [Element "x" [(Name "xmlns" Nothing Nothing, [ContentText "http://jabber.org/protocol/muc"])] []]
           }) sess
    return sess
