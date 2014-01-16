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
import Data.List.Split
import Data.Map()
import Data.Aeson
import Data.Text()
import Data.Text.Encoding
import Data.ByteString.Lazy (toStrict)

import Model.CGroupModel
import Util.CGroupUtil

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

-- |List the tasks currently classified in a specific cgroup
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

-- |Classify a specific task into a specific cgroup 
--
-- Url pattern: http:\/\/{baseurl}\/add-task-to-cgroup\/{subsystem}\/{cgroup}\/{taskId}
--
-- If the request fails an error is thrown, this results in an HTTP error code
--
getClassifyTaskInCGroupR :: String -> String -> String -> Handler String
getClassifyTaskInCGroupR subsystemIn cgroupIn taskIdIn = do
 result <- liftIO $ readProcess "cgclassify" ["-g", subsystemIn ++ [':'] ++ cgroupIn, taskIdIn] ""
 return ("Task " ++ taskIdIn ++ " successfully classified into " ++ subsystemIn ++ [':'] ++ cgroupIn ++ result)