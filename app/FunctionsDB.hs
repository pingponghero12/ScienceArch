{-# LANGUAGE OverloadedStrings #-}

module FunctionsDB where

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
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

data Activity = Activity
  { activityId :: Int,
    userIdData :: Int,
    username :: Text,
    readState :: Maybe Text,
    paperTitle :: Maybe Text,
    timestamp :: UTCTime
  }

data Post = Post
  { postId :: Int,
    userId :: Int,
    usernameData :: Text,
    title :: Text,
    content :: Text,
    timestampData :: UTCTime
  }

data Reading = Reading
  { paperId :: Int,
    titleData :: Text,
    timesRead :: Int
  }

intToText :: Int -> Text
intToText = B.toLazyText . B.decimal

addUser :: Connection -> Text -> Text -> Text -> IO ()
addUser conn userName email password = do
  execute conn "CALL CreateUserProcedure(?, ?, ?)" (email :: Text, password :: Text, userName :: Text)
  return ()

insertPaperSubmission :: Connection -> Int -> Text -> Text -> Text -> Text -> IO ()
insertPaperSubmission conn userId title description format originalTitle = do
  execute
    conn
    "INSERT INTO PAPER_SUBMISSIONS (user_id, title, description, format) \
    \VALUES (?, ?, ?, ?)"
    (userId :: Int, title :: Text, description :: Text, format :: Text)
  return ()

getUsers :: Connection -> IO [(Int, Text)]
getUsers conn = do
  results <- query_ conn "SELECT * FROM AllUsers"
  return $ map (\(user_id, username) -> (user_id, username)) results

getUserName :: Connection -> Int -> IO Text
getUserName conn user_id = do
  results <- query conn "SELECT username FROM AllUsers WHERE user_id = ?" (Only user_id)
  return $ case results of
    (Only txt : _) -> txt
    _ -> ""

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

getUserReadlistByUsername :: Connection -> Text -> IO [(Int, Text)]
getUserReadlistByUsername conn userNameParam = do
  query
    conn
    "SELECT p.paper_id, p.title \
    \FROM PAPERS p \
    \JOIN PAPERS_USER pu ON p.paper_id = pu.paper_id \
    \JOIN USERS u ON u.user_id = pu.user_id \
    \WHERE u.username = ?"
    (Only userNameParam)

getTopPapers :: Connection -> Int -> IO [(Int, Text, Int)]
getTopPapers conn limitCount = do
  -- Sort by popularity
  query
    conn
    "SELECT paper_id, title, popularity \
    \FROM PAPERS \
    \ORDER BY popularity DESC \
    \LIMIT ?"
    (Only limitCount)

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
