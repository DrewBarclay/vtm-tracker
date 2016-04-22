{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Main where

import Web.Scotty.Trans
import Data.Aeson (FromJSON, ToJSON, Value(Null)) 
import GHC.Generics
import Network.Wai.Middleware.Static
import System.FilePath ((</>))
import Paths_vtmtracker --for getDataDir
import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Sql
import Database.Persist.Sqlite ((==.))
import Database.Persist.TH
import Control.Lens
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Text (pack, Text)
import qualified Data.Text.Lazy as TL
import Control.Monad.Logger (runNoLoggingT)
import Network.HTTP.Types.Status (created201, internalServerError500, notFound404)

share [mkPersist sqlSettings {mpsGenerateLenses = True, mpsPrefixFields = False}, mkMigrate "migrateAll"] [persistLowerCase|
Vampire json
    name String
    blood Int
    maxBlood Int
    willpower Int
    maxWillpower Int
    day String
    notes String
    deriving Show Generic
|]

emptyVamp = Vampire "New Vampire" 0 10 0 10 "" ""

data Config = Config { 
  pool :: Sql.ConnectionPool,
  dataDir :: FilePath
}

newtype ConfigM a = ConfigM { 
  runConfigM :: ReaderT Config IO a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

runDB q = do
  p <- lift (asks pool)
  liftIO $ Sql.runSqlPool q p

main = do
    putStrLn "Starting Server..."
    ddir <- fmap (</> "static") getDataDir
    putStrLn $ "Looking in directory " ++ ddir ++ " for files... "
    p <- runNoLoggingT $ Sql.createSqlitePool (pack $ ddir </> "database.sqlite") 8
    let conf = Config { pool = p, dataDir = ddir }

    flip runReaderT conf $ runIdentityT $ do
      runDB $ Sql.runMigration migrateAll
      --runDB $ Sql.insert $ emptyVamp & name .~ "Testpire1"
      --runDB $ Sql.insert $ emptyVamp & name .~ "Testpire2"

    let r m = runReaderT (runConfigM m) conf
    scottyT 3000 r $ application ddir

type Error = TL.Text

application :: FilePath -> ScottyT Error ConfigM ()
application dir = do 
  get "/vampires" $ do
    vamps <- runDB $ Sql.selectList [] []
    json (vamps :: [Sql.Entity Vampire])
  get "/vampire/:id" $ do
    i <- param "id"
    vamp <- runDB $ Sql.get (toKey i :: VampireId)
    case vamp of
      Just vamp -> json vamp
      Nothing -> status notFound404 >> json Null
  get "/vampire" $ do
    let vamp = emptyVamp
    vamp <- runDB $ Sql.insertEntity (vamp :: Vampire)
    json vamp
  put "/vampire/:id" $ do
    vamp <- jsonData
    i <- param "id"
    runDB $ Sql.repsert (toKey i :: VampireId) vamp
    json vamp
  post "/vampire" $ do
    vamp <- jsonData
    key <- runDB $ Sql.insert (vamp :: Vampire)
    vamp <- runDB $ Sql.get key
    json key
  delete "/vampire/:id" $ do
    i <- param "id"
    runDB $ Sql.delete (toKey i :: VampireId)
    json Null
    
    
  middleware $ staticPolicy (noDots >-> addBase dir)
  get "/" $ file (dir </> "index.html")

toKey i = Sql.toSqlKey (fromIntegral (i :: Integer))
