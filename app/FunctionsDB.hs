{-# LANGUAGE OverloadedStrings #-}

module FunctionsDB where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as T
import Database.MySQL.Simple
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

addUser :: Connection -> Text -> Text -> Text -> IO ()
addUser conn userName email password = do
  execute conn "CALL CreateUserProcedure(?, ?, ?)" (email :: Text, password :: Text, userName :: Text)
  return ()

getUsers :: Connection -> IO [(Int, Text)]
getUsers conn = do
  results <- query_ conn "SELECT * FROM AllUsers"
  return $ map (\(user_id, username) -> (user_id, username)) results

verifyUser :: Connection -> Text -> Text -> IO Int
verifyUser conn email password = do
  liftIO $ putStrLn "Debug: vU1 successful"
  _ <- execute_ conn "SET @userId = 0"
  liftIO $ putStrLn "Debug: vU2 successful"
  _ <- execute conn "CALL AuthorizationProcedure(?, ?, @userId)" (email, password)
  liftIO $ putStrLn "Debug: vU3 successful"

  [Only userId] <- query_ conn "SELECT @userId"
  liftIO $ putStrLn "Debug: vU4 successful"
  return userId
