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
  execute conn "CALL CreateUserProcedure(?, 'dupa', ?)" (userName :: Text, email :: Text)
  return ()

getUsers :: Connection -> IO [(Int, Text)]
getUsers conn = do
  results <- query_ conn "SELECT * FROM AllUsers"
  return $ map (\(user_id, username) -> (user_id, username)) results

verifyUser :: Connection -> Text -> Text -> IO Int
verifyUser conn email password = do
  [Only userId] <- query conn "CALL AuthorizationProcedure(?, ?, @userId); SELECT @userId;" (email, password)
  return userId
