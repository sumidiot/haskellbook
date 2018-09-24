{-# LANGUAGE OverloadedStrings #-}
-- OverloadedStrings allows us to create string literals that may end up being of
-- various types, like String, Text, or ByteString. This is similar to how 0 could
-- be an Int or other numeric types.

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty


alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)
--shortyGen = return "AAA" -- for testing collisions


saveURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

encodeKey :: [Char] -> BC.ByteString
encodeKey = BC.pack

-- convenience wrapper for saveURI, dealing with the string type conversions
saveURISimple :: R.Connection -> [Char] -> TL.Text -> IO (Either R.Reply R.Status)
saveURISimple conn shortURI uri = do
  inUse <- keyInUse conn shortURI
  if inUse then
    return $ Left (R.Error "Please try again")
  else
    saveURI conn (encodeKey shortURI) (encodeUtf8 (TL.toStrict uri))


getURI :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

-- convenience wrapper for result of getURI, to handle both error cases
getExpandedURI :: (Either R.Reply (Maybe BC.ByteString)) -> Either TL.Text TL.Text
getExpandedURI e = case e of
  Left reply -> Left $ TL.pack (show reply)
  Right mbBS -> case mbBS of
                  Nothing -> Left "uri not found"
                  Just bs -> Right $ TL.fromStrict (decodeUtf8 bs)

keyInUse :: R.Connection -> [Char] -> IO Bool
keyInUse conn key = do
  uri <- getURI conn (encodeKey key)
  case getExpandedURI uri of
    Left _ -> return False
    Right _ -> return True


linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\"", shorty, "\">Copy and paste your short URL</a>" ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ "shorty is: ", TL.pack (linkShorty shawty) ]
  -- original source had `TL.pack (show resp)` at the head of this list, but that's funny looking

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri, " wasn't a url, did you forget http://?"]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat [ "<a href=\"", tbs, "\">", tbs, "</a>" ]


app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do -- this block will shorten a uri get parameter
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        shawty <- liftIO shortyGen -- lifting into ActionM Monad, representing code that
                                   -- processes web requests into responses
        -- i thought it was cleaner to not do all the encode/decode here, so move the next 2 lines out
        --let shorty = BC.pack shawty -- conversion to ByteString for Redis
        --    uri' = encodeUtf8 (TL.toStrict uri) -- again, type conversion for Redis
        resp <- liftIO (saveURISimple rConn shawty uri) -- result of Redis interaction
        case resp of
          Left err -> text (TL.pack $ show err) -- err is R.Reply, showable
          Right sh -> html (shortyCreated resp shawty)
        --html (shortyCreated resp shawty) -- original, before we had keyInUse
      Nothing -> text (shortyAintUri uri) -- error response if URI was invalid
  get "/:short" $ do -- path capture, "short" becomes an avaliable parameter
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case getExpandedURI uri of
      Left issue -> text issue
      Right fullUri -> html fullUri
-- the below was the original method of the book
--    case uri of
--      Left reply -> text (TL.pack (show reply)) -- some sort of failure, usually an error
--      Right mbBS -> case mbBS of
--        Nothing -> text "uri not found" -- key wasn't in Redis yet
--        Just bs -> html (shortyFound tbs)
--          where tbs :: TL.Text
--                tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo -- localhost:6379
  scotty 3000 (app rConn)
