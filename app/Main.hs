{-# LANGUAGE OverloadedStrings #-}

module Main where

import ConnectDB
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Default
import Data.Maybe (isJust)
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
import RenderUI
import Web.ClientSession (decrypt, encryptIO, randomKey)
import Web.Scotty

type UserSession = Maybe (Int, String, Int) -- (userId, username, admin[0->not admin, 1->admin])

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

getSession :: Key (Session IO BS.ByteString UserSession) -> ActionM (Maybe (Int, String, Int))
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
      html =<< liftIO (T.readFile "templates/base.html")

    get "/home" $ do
      html =<< liftIO (T.readFile "templates/home.html")

    -- API for HTMX
    get "/nav-links" $ do
      maybeSession <- getSession sessionKey
      liftIO $ putStrLn $ "Debug nav-links: maybeSession = " ++ show maybeSession
      case maybeSession of
        Nothing -> do
          liftIO $ putStrLn "Session not found"
          html $ renderNavLinksTemplate False ""
        Just (_, username, _) -> do
          liftIO $ putStrLn "Seession found"
          html $ renderNavLinksTemplate True (B.toLazyText $ B.fromString username)

    get "/auth-section" $ do
      maybeSession <- getSession sessionKey
      html $ renderAuthSectionTemplate (isJust maybeSession)

    get "/reading-list" $ do
      maybeSession <- getSession sessionKey
      readings <- case maybeSession of
        Nothing -> return []
        Just (uid, _, _) -> liftIO $ getUserReading conn uid 5
      html $ renderReadingListTemplate readings

    get "/activity-posts" $ do
      activities <- liftIO $ getLatestActivities conn 20
      posts <- liftIO $ getLatestPosts conn 20
      html $ renderActivityPostsTemplate activities posts

    get "/login" $ do
      html =<< liftIO (T.readFile "templates/login.html")

    --    post "/login" $ do
    --      email <- param @Text "email"
    --      password <- param @Text "password"
    --      liftIO $ putStrLn "Debug: Read successful"
    --
    --      userValid <- liftIO $ verifyUser conn email password
    --
    --      liftIO $ putStrLn "Debug: UserValid succesful"
    --      case userValid of
    --        (0, _, _) -> html "Login unsucceful"
    --        _ -> do
    --          session sessionKey $ Just userValid
    --          html "Login succesful"
    post "/login" $ do
      email <- param @Text "email"
      password <- param @Text "password"
      liftIO $ putStrLn "Debug: Received email and password."

      -- Change 1: Add exception handling for verifyUser
      eUserValid <- liftIO $ try $ verifyUser conn email password :: ActionM (Either SomeException (Int, String, Int))

      case eUserValid of
        Left ex -> do
          -- Change 2: Log the exception and return an error message
          liftIO $ putStrLn $ "Debug: Exception in verifyUser: " ++ show ex
          html "Login failed due to a server error."
        Right userValid@(userId, username, admin) -> do
          liftIO $ putStrLn $ "Debug: verifyUser returned: " ++ show userValid
          case userValid of
            (0, _, _) -> html "Login unsuccessful"
            _ -> do
              session sessionKey $ Just userValid
              html "Login successful"

    post "/logout" $ do
      session sessionKey Nothing
      html "Logged out"

    get "/profile" $ do
      maybeSession <- getSession sessionKey
      case maybeSession of
        Nothing -> redirect "/login"
        Just (_, username, _) ->
          do html $ B.toLazyText (B.fromString "Welcome: " <> B.fromString username)

    get "/register" $ do
      html =<< liftIO (T.readFile "templates/register.html")

    post "/register" $ do
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

    get "/users/:username/readlist" $ do
      userNameParam <- param @Text "username"
      readlist <- liftIO $ getUserReadlistByUsername conn userNameParam
      liftIO $ putStrLn ("Debug: /users/:username/readlist => " ++ show readlist)
      html =<< liftIO (T.readFile "templates/readlist.html")

    get "/browse" $ do
      -- idk what to do about it, not trying now
      -- will probably move base.html to hs and send with load variable
      isHtmx <- (== Just "true") <$> header "HX-Request"
      if isHtmx
        then do
          html =<< liftIO (T.readFile "templates/browse.html")
        else do
          baseContent <- liftIO (T.readFile "templates/base.html")
          html baseContent
          html =<< liftIO (T.readFile "templates/browse.html")

    get "/papers" $ do
      paperList <- liftIO $ getTopPapers conn 100
      html $ renderPapers paperList

    get "/new-submission" $ do
      maybeSession <- getSession sessionKey
      case maybeSession of
        Nothing -> html "You are not logged in."
        Just n -> html =<< liftIO (T.readFile "templates/new-submission.html")

    post "/submit-paper" $ do
      maybeSession <- getSession sessionKey
      case maybeSession of
        Nothing -> html "You are not logged in."
        Just (0, _, _) -> html "You are not logged in."
        Just (userId, _, _) -> do
          title <- param @Text "title"
          description <- param @Text "description"
          format <- param @Text "format"
          originalTitle <- param @Text "original_title"

          liftIO $ insertPaperSubmission conn userId title description format originalTitle
          html "Submission created successfully!"

    notFound $ do
      text "Error 404 - file not found"
