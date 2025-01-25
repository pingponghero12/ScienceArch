{-# LANGUAGE OverloadedStrings #-}

module Main where

import ConnectDB
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as T
import Database.MySQL.Simple
import FunctionsDB
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

main :: IO ()
main = do
  conn <- connect connInfo
  putStrLn "Debug: Database connection established"
  scotty 3000 $ do
    middleware $ staticPolicy (addBase "static")

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
      liftIO $ do
        putStrLn $ "Debug: userName = " ++ show userName
        putStrLn $ "Debug: email = " ++ show email
        addUser conn userName email
      html "User has been added!"

    get "/users" $ do
      users <- liftIO $ getUsers conn
      json users
