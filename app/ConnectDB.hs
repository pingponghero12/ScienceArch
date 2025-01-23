module ConnectDB where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as T
import Database.MySQL.Simple
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty

connInfo :: ConnectInfo
connInfo =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectPort = 3306,
      connectUser = "admin",
      connectPassword = "1234",
      connectDatabase = "papers_db"
    }
