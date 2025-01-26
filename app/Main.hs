{-# LANGUAGE OverloadedStrings #-}

module Main where

import ConnectDB
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Default
import Data.Text.Lazy (Text, toStrict)
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as B
import Data.Text.Lazy.Builder.RealFloat qualified as B
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as T
import Data.Vault.Lazy (Key, Vault, lookup, newKey)
import Data.Vault.Lazy qualified as Vault
import Database.MySQL.Simple
import FunctionsDB
import Network.Wai (Middleware, vault)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Session (Session, SessionStore, withSession)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Network.Wai.Session.Map (mapStore_)
import Web.ClientSession (decrypt, encryptIO, randomKey)
import Web.Scotty

type UserSession = Maybe Int -- user_id

intToText :: Int -> Text
intToText = B.toLazyText . B.decimal

session :: Key (Session IO BS.ByteString UserSession) -> UserSession -> ActionM UserSession
session key val = do
  vaultVal <- vault <$> request
  case Vault.lookup key vaultVal of
    Nothing -> error "Session not found"
    Just (sessionLookup, sessionInsert) -> do
      case val of
        Nothing -> do
          -- Need to provide a key for lookup
          result <- liftIO $ sessionLookup "session"
          return $ maybe Nothing id result
        Just v -> do
          -- Need to provide both key and value for insert
          liftIO $ sessionInsert "session" (Just v)
          return (Just v)

getSession :: Key (Session IO BS.ByteString UserSession) -> ActionM (Maybe Int)
getSession key = do
  vaultVal <- vault <$> request
  case Vault.lookup key vaultVal of
    Nothing -> return Nothing
    Just (sessionLookup, _) -> do
      result <- liftIO $ sessionLookup "session"
      return $ case result of
        Just (Just v) -> Just v
        _ -> Nothing

main :: IO ()
main = do
  conn <- connect connInfo
  putStrLn "Debug: Database connection established"

  -- Sesion key and store
  sessionKey <- newKey
  sessionStore <- mapStore_ :: IO (SessionStore IO BS.ByteString UserSession)

  let sessionMiddleware = withSession sessionStore "session" def sessionKey

  scotty 3000 $ do
    middleware $ staticPolicy (addBase "static")
    middleware sessionMiddleware

    get "/" $ do
      liftIO $ putStrLn "Debug: / in"
      maybeUserId <- getSession sessionKey
      activities <- liftIO $ getLatestActivities conn 20
      posts <- liftIO $ getLatestPosts conn 20
      readingList <- case maybeUserId of
        Nothing -> return []
        Just uid -> liftIO $ getUserReading conn uid 5

      liftIO $
        putStrLn
          ( "Debug: Home => Activities: "
              ++ show activities
              ++ ", Posts: "
              ++ show posts
              ++ ", Reading: "
              ++ show readingList
          )
      liftIO $ putStrLn "Debug: / last"
      html =<< liftIO (T.readFile "templates/home.html")

    get "/login" $ do
      html =<< liftIO (T.readFile "templates/login.html")

    post "/login" $ do
      email <- param @Text "email"
      password <- param @Text "password"
      liftIO $ putStrLn "Debug: Read successful"

      userValid <- liftIO $ verifyUser conn email password

      liftIO $ putStrLn "Debug: UserValid succesful"
      case userValid of
        0 -> html "Login unsucceful"
        userId -> do
          session sessionKey $ Just userValid
          html "Login succesful"

    post "/logout" $ do
      session sessionKey Nothing
      html "Logged out"

    get "/profile" $ do
      maybeUserId <- getSession sessionKey
      case maybeUserId of
        Nothing -> redirect "/login"
        Just userId ->
          do html $ "Welcome: " <> intToText userId

    post "/add_user" $ do
      liftIO $ putStrLn "Debug: Received POST /add_user request"
      userName <- param @Text "username"
      email <- param @Text "email"
      password <- param @Text "password"
      liftIO $ do
        putStrLn $ "Debug: userName = " ++ show userName
        putStrLn $ "Debug: email = " ++ show email
        putStrLn $ "Debug: password = " ++ show password
        addUser conn userName email password
      html "User has been added!"

    get "/users" $ do
      users <- liftIO $ getUsers conn
      json users

    get "/users/:username" $ do
      userNameParam <- param @Text "username"
      maybeUserInfo <- liftIO $ getUserInfoByUsername conn userNameParam
      case maybeUserInfo of
        Nothing -> html "User not found"
        Just (userId, userName, desc, img) -> do
          liftIO $ putStrLn ("Debug: /users/:username => " ++ show userName)
          html =<< liftIO (T.readFile "templates/user.html")
