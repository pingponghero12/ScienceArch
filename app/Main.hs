{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO qualified as T
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
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
