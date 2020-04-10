{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Control.Exception
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock (UTCTime)
import Database.MySQL.Base (MySQLError)
import Database.MySQL.Simple hiding (connect)
import qualified Database.MySQL.Simple as D (connect)
import System.Environment (lookupEnv)

-- TODO: newtype all of this crap
type DatabaseInfo = (String, String, String) -- db, user, pw
type IrcInfo = (String, String, String, String) -- server, channel, nick, nickserv pw
type Shorturl = (Integer, B.ByteString, B.ByteString, B.ByteString, UTCTime, Bool, Maybe Bool, Maybe B.ByteString)

genericTry :: MIrc -> B.ByteString -> IO () -> IO ()
genericTry s chan ioa =
  catch ioa (\e -> do
                let err = show (e :: MySQLError)
                sendMsg s chan (B.pack ("Sorry, I oopsed: " ++ err)))

onMessage :: String -> EventFunc
onMessage channelStr s m
  | msg == "!ping" && chan == cfgChan = do
      sendMsg s chan "pang"
  | "!disable " `B.isPrefixOf` msg && chan == cfgChan = do
      let arg = B.drop (B.length "!disable ") msg
      tryMsg $ dropSingle s chan arg
  | "!disableip " `B.isPrefixOf` msg && chan == cfgChan = do
      let arg = B.drop (B.length "!disableip ") msg
      tryMsg $ dropIp s chan arg
  | "!coshorten " `B.isPrefixOf` msg && chan == cfgChan = do
      let arg = B.drop (B.length "!coshorten ") msg
      tryMsg $ coshorten s chan arg
  | "!banip " `B.isPrefixOf` msg && chan == cfgChan = do
      let arg = B.drop (B.length "!banip ") msg
      tryMsg $ banIp s chan arg
  | "!unbanip " `B.isPrefixOf` msg && chan == cfgChan = do
      let arg = B.drop (B.length "!unbanip ") msg
      tryMsg $ unbanIp s chan arg
  | otherwise = putStrLn $ show m
  where chan = fromJust $ mChan m
        cfgChan = B.pack channelStr
        msg = mMsg m
        tryMsg = genericTry s chan

onNumeric :: String -> String -> EventFunc
onNumeric nick pw s m
  | mCode m == "001" = do
      sendMsg s "nickserv" (B.unwords ["identify", B.pack nick, B.pack pw])
  | otherwise = return ()

onNotice :: String -> EventFunc
onNotice channel s m
  | mOrigin m == (Just "NickServ") = sendCmd s (MJoin (B.pack channel) Nothing)
  | otherwise = return ()

events :: IrcInfo -> [IrcEvent]
events (server, channel, nick, pass) =
  [ Privmsg (onMessage channel)
  , Numeric (onNumeric nick pass)
  , Notice (onNotice channel)
  ]

-- TODO: newtype these
-- TODO: don't partial match
-- server, channel, nick, nickserv pw
ircInfo :: IO IrcInfo
ircInfo = do
  Just server <- lookupEnv "DAGDBOT_IRCSERVER"
  Just channel <- lookupEnv "DAGDBOT_IRCCHANNEL"
  Just nick <- lookupEnv "DAGDBOT_IRCNICKNAME"
  Just pass <- lookupEnv "DAGDBOT_IRCPASSWORD"
  return (server, channel, nick, pass)

ircConfig :: IrcInfo -> IrcConfig
ircConfig conf@(server, channel, nick, pass) =
  (mkDefaultConfig server nick)
  { cChannels = [channel]
  , cEvents   = events conf
  }

-- TODO: newtype these
-- TODO: don't partial match
-- database, username, password
dbInfo :: IO DatabaseInfo
dbInfo = do
  Just db <- lookupEnv "DAGDBOT_DATABASE"
  Just username <- lookupEnv "DAGDBOT_USERNAME"
  Just password <- lookupEnv "DAGDBOT_PASSWORD"
  return (db, username, password)

dagddb :: DatabaseInfo -> IO Connection
dagddb (db, username, password) =
  D.connect defaultConnectInfo { connectDatabase = db
                               , connectUser = username
                               , connectPassword = password
                               }

dropSingle :: MIrc -> B.ByteString -> B.ByteString -> IO ()
dropSingle s chan url = do
  conn <- dbInfo >>= dagddb
  updated <- execute conn "update shorturls set enabled=0 where shorturl=?" [url]
  let res = case updated of
              0 -> "Nothing to do."
              n -> "Updated " <> B.pack (show n) <> " rows."
  leftover <- query conn "select * from shorturls where enabled=1 and owner_ip=(select B.owner_ip from (select shorturl, owner_ip from shorturls) as B where B.shorturl=?)" [url] :: IO [Shorturl]
  case length leftover of
    0 -> sendMsg s chan (res <> " No other shorturls are currently enabled from the author's IP.")
    n -> do
      sendMsg s chan (res <> " " <> B.pack (show n) <> " other shorturls are currently enabled from the author's IP. Preview follows (type !disableip <original shorturl code> to disable all):")
      preview s chan (take 10 leftover)

dropIp :: MIrc -> B.ByteString -> B.ByteString -> IO ()
dropIp s chan url = do
  conn <- dbInfo >>= dagddb
  updated <- execute conn "update shorturls set enabled=0 where enabled=1 and owner_ip=(select B.owner_ip from (select shorturl, owner_ip from shorturls) as B where B.shorturl=?)" [url]
  sendMsg s chan ("Updated " <> B.pack (show updated) <> " rows.")

banIp :: MIrc -> B.ByteString -> B.ByteString -> IO ()
banIp s chan url = do
  conn <- dbInfo >>= dagddb
  res <- listToMaybe <$> query conn "select * from shorturls where shorturl=?" [url] :: IO (Maybe Shorturl)
  case res of
    Nothing -> sendMsg s chan "No such shorturl found."
    Just (_, _, _, ip, _, _, _, _) -> do
      updated <- execute conn "insert into blocked_ips(ip_start, ip_end) values(inet6_aton(?), inet6_aton(?))" [ip, ip]
      case updated of
        1 -> sendMsg s chan "Blocked."
        _ -> sendMsg s chan "Sorry, I oopsed."

unbanIp :: MIrc -> B.ByteString -> B.ByteString -> IO ()
unbanIp s chan url = do
  conn <- dbInfo >>= dagddb
  res <- listToMaybe <$> query conn "select * from shorturls where shorturl=?" [url] :: IO (Maybe Shorturl)
  case res of
    Nothing -> sendMsg s chan "No such shorturl found."
    Just (_, _, _, ip, _, _, _, _) -> do
      updated <- execute conn "delete from blocked_ips where ip_start=inet6_aton(?) and ip_end=inet6_aton(?)" [ip, ip]
      case updated of
        1 -> sendMsg s chan "Unblocked."
        _ -> sendMsg s chan "Sorry, I oopsed."

preview :: MIrc -> B.ByteString -> [Shorturl] -> IO ()
preview s chan leftover =
  mapM_ (sendMsg s chan . formatShortUrl) leftover
{-# INLINE preview #-}

formatShortUrl :: Shorturl -> B.ByteString
formatShortUrl (idnum, short, long, ip, date, enabled, custom, hash) =
  B.pack (show date) <> " - " <> short <> ": " <> long
{-# INLINE formatShortUrl #-}

coshorten :: MIrc -> B.ByteString -> B.ByteString -> IO ()
coshorten s chan url = do
  conn <- dbInfo >>= dagddb
  res <- listToMaybe <$> query conn "select * from shorturls where shorturl=? and enabled=1" [url] :: IO (Maybe Shorturl)
  case res of
    Nothing -> sendMsg s chan "No such (enabled) shorturl found."
    Just surl -> sendMsg s chan (formatShortUrl surl)

{-
Single match:
!disable foo
> foo disabled. No other shorturls created by foo's owner_ip.

Multiple matches:
!disable bar
> bar disabled. 8 other shorturls found created by bar's owner_ip. Issue
  "!ip bar" to list them, and "!disableip bar" to disable them all.
-}

main :: IO (Either IOError MIrc)
main = do
  info <- ircInfo
  connect (ircConfig info) False True
