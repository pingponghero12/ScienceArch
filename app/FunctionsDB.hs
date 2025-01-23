{-# LANGUAGE OverloadedStrings #-}

module FunctionsDB where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as T
import Database.MySQL.Simple
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

addUser :: Connection -> Text -> Text -> IO ()
addUser conn userName email = do
  execute conn "INSERT INTO users (username, email) VALUES (?, ?)" (userName :: Text, email :: Text)
  return ()

getUsers :: Connection -> IO [(Text, Text)]
getUsers conn = do
  results <- query_ conn "SELECT username, email FROM users"
  return $ map (\(username, email) -> (decodeUtf8 username, decodeUtf8 email)) results
