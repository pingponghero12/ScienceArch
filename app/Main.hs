{-# LANGUAGE OverloadedStrings #-}

module Main where

import ConnectDB
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Default
import Data.Text.Lazy (Text)
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

    get "/login" $ do
      email <- param @Text "email"
      password <- param @Text "password"

      userValid <- liftIO $ verifyUser conn email password

      case userValid of
        0 -> html "Login unsucceful"
        userId -> do
          session sessionKey $ Just userValid
          html "Login succesful"

    get "/logout" $ do
      session sessionKey Nothing
      html "Logged out"

    get "/" $ do
      html =<< liftIO (T.readFile "templates/home.html")

    get "/list" $ do
      html =<< liftIO (T.readFile "templates/user_list.html")

    get "/search" $ do
      html =<< liftIO (T.readFile "templates/search.html")

    get "/article/:id" $ do
      artId <- param @Text "id"
      html =<< liftIO (T.readFile "templates/article.html")

    get "/user/:username" $ do
      username <- param @Text "username"
      html =<< liftIO (T.readFile "templates/user.html")

    get "/author/:authorId" $ do
      authorId <- param @Text "authorId"
      html =<< liftIO (T.readFile "templates/author.html")

    post "/add_user" $ do
      liftIO $ putStrLn "Debug: Received POST /add_user request"
      userName <- param @Text "username"
      email <- param @Text "email"
      password <- param @Text "password"
      liftIO $ do
        putStrLn $ "Debug: userName = " ++ show userName
        putStrLn $ "Debug: email = " ++ show email
        putStrLn $ "Debug: password = " ++ show password
        addUser conn userName email
      html "User has been added!"

    get "/users" $ do
      users <- liftIO $ getUsers conn
      json users
