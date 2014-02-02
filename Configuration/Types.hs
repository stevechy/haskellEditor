{-# LANGUAGE DeriveGeneric #-}

module Configuration.Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)


data Configuration = Configuration { rootFolder :: String, cabalFile :: String, commands :: Maybe [[String]] }
    deriving (Show, Generic)

instance ToJSON Configuration

instance FromJSON Configuration






