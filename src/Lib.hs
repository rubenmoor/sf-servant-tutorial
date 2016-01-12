{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startServer
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (ToJSON)
import           GHC.Generics             (Generic)

import           Network.Wai              (Application)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant                  ((:>), Get, JSON, Proxy (Proxy),
                                           Server, serve)

startServer :: IO ()
startServer = Warp.run 3000 app

-- schema

data User = User
  { userName  :: String
  , userEmail :: String
  } deriving (Generic)

-- a bug somewhere else that generates a warning
instance ToJSON User

-- instad of a database

alice :: User
alice = User "Alice" "alice@web.de"

bob :: User
bob = User "Robert" "robert@gmx.net"

-- database action
getUsers :: IO [User]
getUsers = pure [alice, bob]

-- the app

app :: Application
app = serve api server

-- the API, or the routes

type Api = "users" :> Get '[JSON] [User]

-- Server Api == EitherT ServantErr IO [User]
server :: Server Api
server = liftIO getUsers

api :: Proxy Api
api = Proxy
