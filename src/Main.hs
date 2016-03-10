{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Network.Wai.Middleware.Static
import System.FilePath ((</>))
import Paths_vtmtracker --for getDataDir

data Vampire = Vampire { name :: String, blood :: Int, willpower :: Int, day :: Int, wakeupTime :: String } deriving (Show, Generic)

instance ToJSON Vampire
instance FromJSON Vampire

allVampires = [Vampire { name = "Testpire1", blood = 0, willpower = 0, day = 0, wakeupTime = "" },
               Vampire { name = "Testpire2", blood = 0, willpower = 0, day = 0, wakeupTime = "" }
              ]

main = do
    putStrLn "Starting Server..."
    dataDir <- fmap (</> "static") getDataDir
    scotty 3000 $ do
        get "/hello" $ do
            text "hello world!"
        get "/users" $ do
          json allVampires
        middleware $ staticPolicy (noDots >-> addBase dataDir)
        get "/" $ file (dataDir </> "index.html")


