{-# LANGUAGE OverloadedStrings #-}

module RenderUI where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Lazy.Builder.Int qualified as B
import Data.Text.Lazy.Builder.RealFloat qualified as B
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as T
import Data.Time (UTCTime)
import Database.MySQL.Simple
import FunctionsDB
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

renderPapers :: [(Int, Text, Int)] -> Text
renderPapers papers =
  TL.concat $
    map
      ( \(pid, title, pop) ->
          TL.concat
            [ "<div style='margin-bottom: 5px; \
              \background-color: #262626; \
              \padding: 10px;'>",
              "<strong>Title: </strong>",
              title,
              " ",
              "<strong>Popularity: </strong>",
              TL.pack (show pop),
              "</div>"
            ]
      )
      papers

renderNavLinksTemplate :: Bool -> Text -> Text
renderNavLinksTemplate isLoggedIn username =
  if isLoggedIn
    then
      mconcat
        [ "<a href=\"/users/",
          username,
          "\">",
          username,
          "</a>",
          "<a href=\"/users/",
          username,
          "/readlist\">Reading List</a>"
        ]
    else ""

renderAuthSectionTemplate :: Bool -> Text
renderAuthSectionTemplate isLoggedIn =
  if isLoggedIn
    then "<a href=\"/settings\">Settings</a>"
    else
      mconcat
        [ "<a href=\"#\" hx-get=\"/login\" hx-target=\"#content\" hx-swap=\"inerHTML\" hx-push-url=\"true\">Log In</a>",
          "   ",
          "<a href=\"#\" hx-get=\"/register\" hx-target=\"#content\" hx-swap=\"inerHTML\" hx-push-url=\"true\" >Sign up</a>"
        ]

renderReadingListTemplate :: [(Int, Text)] -> Text
renderReadingListTemplate [] = "<div class=\"item-row\">No current readings</div>"
renderReadingListTemplate readings =
  mconcat
    [ mconcat
        [ "<div class=\"item-row\">",
          "<a href=\"/papers/",
          intToText paperId,
          "\">",
          title,
          "</a>",
          "</div>"
        ]
      | (paperId, title) <- readings
    ]

renderActivityPostsTemplate :: [(Int, Int, Maybe Text)] -> [(Int, Int, Text, Text)] -> Text
renderActivityPostsTemplate activities posts =
  mconcat
    [renderActivities activities, renderPosts posts]
  where
    renderActivities :: [(Int, Int, Maybe Text)] -> Text
    renderActivities acts = mconcat $ map renderActivity acts

    renderActivity :: (Int, Int, Maybe Text) -> Text
    renderActivity (actId, userId, state) =
      mconcat
        [ "<div class=\"item-row activity\">",
          "<p>Activity ",
          intToText actId,
          " by User ",
          intToText userId,
          maybe "" (" - " <>) state,
          "</p>",
          "</div>"
        ]

    renderPosts :: [(Int, Int, Text, Text)] -> Text
    renderPosts ps = mconcat $ map renderPost ps

    renderPost :: (Int, Int, Text, Text) -> Text
    renderPost (_, userId, title, content) =
      mconcat
        [ "<div class=\"item-row post\">",
          "<h3>",
          title,
          "</h3>",
          "<p>",
          content,
          "</p>",
          "<small>Posted by User ",
          intToText userId,
          "</small>",
          "</div>"
        ]
