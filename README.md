Create and clone empty repository

    $ git clone ssh://user@gitlab.net/sf-servant-tutorial.git

Initialize new project with `stack`

    $ cd sf-servant-tutorial
    $ stack new sf-servant-tutorial --bare new-template --resolver lts-4.1
    $ stack build # just for fun
    $ stack exec sf-servant-tutorial-exe

Edit files to run a web server

    -- app/Main.hs

    module Main where

    import Lib

    main :: IO ()
    main = startServer

    -- src/Lib.hs
 
    {-# LANGUAGE OverloadedStrings #-}

    module Lib
        ( startServer
        ) where

    import           Network.HTTP.Types       (status200)
    import           Network.Wai              (Request, Response, responseBuilder)
    import qualified Network.Wai.Handler.Warp as Warp

    startServer :: IO ()
    startServer = Warp.run 3000 app

    app :: Request -> (Response -> t) -> t
    app _ respond = respond $ responseBuilder
                              status200
                              [("Content-Type", "text/plain")]
                              "hello world"
   

    -- sf-servant-tutorial.cabal

    -- ...
    library
      hs-source-dirs:      src
      exposed-modules:     Lib
      build-depends:       base >= 4.7 && < 5
                         , http-types
                         , wai
                         , warp
      default-language:    Haskell2010
    -- ...

Check if everythings works as expected

    $ stack exec sf-servant-tutorial-exe

Inside a browser, go to

    http://localhost:3000/

Now add servant to serve an API.
Starting with required packages:

    -- sf-servant-tutorial.cabal

    -- ...
    library
      hs-source-dirs:      src
      exposed-modules:     Lib
      build-depends:       base >= 4.7 && < 5
                         , aeson
                         , servant-server
                         , transformers
                         , wai
                         , warp
      default-language:    Haskell2010
    -- ...

    -- src/Lib.hs

Compiler extensions.
Servant uses `DataKinds` for compile-time check of the content-types.
`DeriveGeneric` for generic JSON instance.
Servant uses `TypeOperators` to allow operator syntax in type definitions. 

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

Now serving an `app :: Application`, taken care of by `servant`.

    startServer :: IO ()
    startServer = Warp.run 3000 app

Mocking some data entries.

    -- schema

    data User = User
      { userName  :: String
      , userEmail :: String
      } deriving (Generic)

    -- a ghc bug generates a warning here
    instance ToJSON User

    -- instad of a database

    alice :: User
    alice = User "Alice" "alice@web.de"

    bob :: User
    bob = User "Robert" "robert@gmx.net"

    -- imagine a database action
    getUsers :: IO [User]
    getUsers = pure [alice, bob]


Now build the actual "servant application"

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

Why is `Proxy` needed here? Further information can be found in [this stackoverflow question](http://stackoverflow.com/questions/31636431/data-proxy-in-servants-public-api-why-proxy-with-scopedtypevariables-doesnt-w).
