{-# LANGUAGE DeriveGeneric #-}
-- |A restful controller to provide the capability to view available cgroups, 
-- retrieve tasks for a cgroup and classify a specific task to a cgroup
--
-- Examples for parameters:
--
-- {baseurl} - http:\/\/example.org\/tobias-cgroup-manager\/
--
-- {subsystem} - "name=systemd"
--
-- {cgroup} - "system\/display-manager.service"
--
-- {taskId} - "1009"
--
-- WARNING: Make sure you URL encode your parameters if calling from a browser
-- i.e http:\/\/example.org\/tobias-cgroup-manager\/list-tasks-for-cgroup\/system%2Fdisplay-manager.service

module Handler.CGroupManager where

import Import
import System.Environment
import System.Process
import Data.List
import Data.List.Split
import Data.Map()
import GHC.Generics
import Data.Char(isDigit)
import Data.Aeson
import Data.Text()
import Data.Text.Encoding
import Data.ByteString.Lazy (toStrict)

-- |List all available cgroups
--
-- Url pattern: http:\/\/{baseurl}\/list-available-cgroups\/
--
getAvailableCGroupsR :: Handler Text
getAvailableCGroupsR = do
 lscgroupExecutable <- liftIO $ getEnv "LSC_GROUP_EXECUTABLE"
 result <- liftIO $ readProcess lscgroupExecutable [] ""
 let sourceData = Data.List.Split.splitOn "\n" result
 let availableProcessGroupList = [AvailableProcessGroup (getSubSystem x) (getCGroup x) | x <- sourceData, x /= ""]
 let jsonResult = decodeUtf8 (toStrict (encode availableProcessGroupList))
 return jsonResult

data AvailableProcessGroup = AvailableProcessGroup { subsystem :: String, cgroup :: String } deriving (Generic, Show)
instance ToJSON AvailableProcessGroup

getSubSystem :: String -> String
getSubSystem [] = []
getSubSystem (x:xs)
 | x /= ':' = x : getSubSystem xs
 | otherwise = []

getCGroup :: String -> String
getCGroup [] = []
getCGroup (x:xs)
 | x == ':' = xs
 | otherwise = getCGroup xs

-- |Get the tasks currently classified in a specific cgroup
--
-- Url pattern: http:\/\/{baseurl}\/list-tasks-for-cgroup\/{cgroup}
-- 
getTasksForCGroupR :: String -> Handler Text
getTasksForCGroupR cgroupIn = do
 processResult <- liftIO $ readProcess "systemd-cgls" [cgroupIn] ""
 let rawSourceData = Data.List.Split.splitOn "\n" processResult 
 let sourceData =  formatSourceLinePositions rawSourceData
 let processGroup = parseProcessGroup sourceData
 let jsonResult = decodeUtf8 (toStrict (encode processGroup))
 return jsonResult

data ProcessGroup = ProcessGroup { 
  name ::String
  , descendants :: [ProcessGroup]
  } | Task {id :: String, description :: String} deriving (Generic, Show)
instance ToJSON ProcessGroup

parseProcessGroup :: [String] -> ProcessGroup
parseProcessGroup [] = error "No process groups found"
parseProcessGroup (x:xs)
 | otherwise = ProcessGroup nameWithoutPosition (parseProcessGroupChildren childrenLocal)
 where firstNumber = read ((head x):[]) :: Int
       childrenLocal = sourceChildren firstNumber xs
       nameWithoutPosition = tail x

parseProcessGroupChildren :: [String] -> [ProcessGroup]
parseProcessGroupChildren [] = []
parseProcessGroupChildren (x:xs)
 | length xs == 0 = [Task taskId taskDescription]
 | length siblingsLocal == 0 = [ProcessGroup nameWithoutPosition (parseProcessGroupChildren childrenLocal)]
 | length childrenLocal == 0 = (Task (tail x) (tail x)):parseProcessGroupChildren siblingsLocal
 | otherwise = ProcessGroup x (parseProcessGroupChildren childrenLocal): parseProcessGroupChildren siblingsLocal
 where firstNumber = read ((head x):[]) :: Int
       taskId = getTaskId (tail x)
       nameWithoutPosition = tail x
       taskDescription = getTaskDescription (tail x)
       siblingsLocal = sourceSiblings firstNumber xs
       childrenLocal = sourceChildren firstNumber xs

sourceChildren :: Int -> [String] -> [String]
sourceChildren _ [] = []
sourceChildren parentPosition (x:xs)
 | firstNumber > parentPosition = x:(sourceChildren parentPosition xs)
 | otherwise = []
 where firstNumber = read ((head x):[]) :: Int

sourceSiblings :: Int -> [String] -> [String]
sourceSiblings _ [] = []
sourceSiblings parentPosition (x:xs)
 | firstNumber > parentPosition = (sourceSiblings parentPosition xs)
 | otherwise = x:xs
 where firstNumber = read ((head x):[]) :: Int

getTaskId :: String -> String
getTaskId [] = []
getTaskId (x:xs)
 | isDigit x = x : getTaskId xs
 | otherwise = []

getTaskDescription :: String -> String
getTaskDescription [] = []
getTaskDescription (x:xs)
 | isDigit x = getTaskDescription xs
 | x == ' ' = getTaskDescription xs
 | otherwise = xs

formatSourceLinePositions :: [String] -> [String]
formatSourceLinePositions [] = []
formatSourceLinePositions (x:xs) 
 | x == [] = []
 | otherwise = (show position ++ drop position x) : formatSourceLinePositions xs
 where position = countPrefix x

countPrefix :: String -> Int
countPrefix [] = 0
countPrefix (x:xs) 
 | x == ' ' = 1 + countPrefix xs
 | x == '├' = 1 + countPrefix xs
 | x == '─' = 1 + countPrefix xs
 | x == '└' = 1 + countPrefix xs
 | x == '│' = 1 + countPrefix xs
 | otherwise = 0

-- |Classify a specific task into a specific cgroup 
--
-- Url pattern: http:\/\/{baseurl}\/add-task-to-cgroup\/{subsystem}\/{cgroup}\/{taskId}
--
getClassifyTaskInCGroupR :: String -> String -> String -> Handler String
getClassifyTaskInCGroupR subsystemIn cgroupIn taskIdIn = do
 result <- liftIO $ readProcess "cgclassify" ["-g", subsystemIn ++ [':'] ++ cgroupIn, taskIdIn] ""
 return ("Task " ++ taskIdIn ++ " successfully classified into " ++ subsystemIn ++ [':'] ++ cgroupIn ++ result)