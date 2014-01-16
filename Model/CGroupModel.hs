{-# LANGUAGE DeriveGeneric #-}
module Model.CGroupModel where

import Import
import Data.Map()
import GHC.Generics
import Data.Text()

data AvailableProcessGroup = AvailableProcessGroup { subsystem :: String, cgroup :: String } deriving (Generic, Show)
instance ToJSON AvailableProcessGroup

data ProcessGroup = ProcessGroup { 
  name ::String
  , descendants :: [ProcessGroup]
  } | Task { 
  id :: String, description :: String
  } deriving (Generic, Show)
instance ToJSON ProcessGroup