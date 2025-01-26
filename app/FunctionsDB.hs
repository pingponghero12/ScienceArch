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

getLatestPosts :: Connection -> Int -> IO [(Int, Int, Text, Text)]
getLatestPosts conn limitCount = do
  query
    conn
    "SELECT post_id, user_id, title, content \
    \FROM POSTS ORDER BY created_at DESC LIMIT ?"
    (Only limitCount)

getLatestActivities :: Connection -> Int -> IO [(Int, Int, Maybe Text)]
getLatestActivities conn limitCount = do
  query
    conn
    "SELECT activity_id, user_id, read_state \
    \FROM ACTIVITIES ORDER BY created_at DESC LIMIT ?"
    (Only limitCount)

getUserReading :: Connection -> Int -> Int -> IO [(Int, Text)]
getUserReading conn userId limitCount = do
  query
    conn
    "SELECT p.paper_id, p.title \
    \FROM PAPERS p \
    \INNER JOIN PAPERS_USER pu ON p.paper_id = pu.paper_id \
    \WHERE pu.user_id = ? \
    \ORDER BY pu.times_read DESC \
    \LIMIT ?"
    (userId, limitCount)

getUserInfoByUsername :: Connection -> Text -> IO (Maybe (Int, Text, Maybe Text, Maybe Text))
getUserInfoByUsername conn userNameParam = do
  results <-
    query
      conn
      "SELECT user_id, username, description, image FROM USERS WHERE username = ? LIMIT 1"
      (Only userNameParam)
  case results of
    [(uid, uname, desc, img)] -> return (Just (uid, uname, desc, img))
    _ -> return Nothing
