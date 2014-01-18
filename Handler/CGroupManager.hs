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
import System.Process
import Data.List.Split
import Data.Map()
import Data.Aeson
import Data.Text()
import Data.Text.Encoding
import Data.ByteString.Lazy (toStrict)
import System.IO.UTF8
import Data.UUID.V1 as UUID (nextUUID)
import qualified Data.UUID
import Data.Maybe

import Model.CGroupModel
import Util.CGroupUtil

-- |List all available cgroups
--
-- Url pattern: http:\/\/{baseurl}\/list-available-cgroups\/
--
getAvailableCGroupsR :: Handler Text
getAvailableCGroupsR = do
 result <- liftIO $ readProcess "lscgroup" [] ""
 let sourceData = Data.List.Split.splitOn "\n" result
 let availableProcessGroupList = [AvailableProcessGroup (getSubSystem x) (getCGroup x) | x <- sourceData, x /= ""]
 let jsonResult = decodeUtf8 (toStrict (encode availableProcessGroupList))
 return jsonResult

-- |List the tasks currently classified in a specific cgroup
--
-- Url pattern: http:\/\/{baseurl}\/list-tasks-for-cgroup\/{cgroup}
-- 
-- Unfortunately this command returns UTF8 which in some cases readProcess cannot handle, so we need to pipe the result to
-- a file and read the result with a utf8 safe reader. The injection protection should work but is worth double checking 
-- yourself.
getTasksForCGroupR :: String -> Handler Text
getTasksForCGroupR cgroupIn = do
 uuid <- liftIO $ UUID.nextUUID -- So no collisions in tmp
 let uuidString = Data.UUID.toString (fromJust uuid)
 let arguments = ['"']++(safetyParse cgroupIn)++['"'] -- Stop injections of other commands
 exitCode <- liftIO $ system ("systemd-cgls " ++ arguments ++ " > /tmp/" ++ uuidString)
 processResult <- liftIO $ readFile ("/tmp/" ++ uuidString)
 exitCode <- liftIO $ system ("rm /tmp/" ++ uuidString)
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
-- We need sudo access here so need to use system. The injection protection should work but is worth double checking
-- yourself.
getClassifyTaskInCGroupR :: String -> String -> String -> Handler String
getClassifyTaskInCGroupR subsystemIn cgroupIn taskIdIn = do
 let safeCgroupLocation = ['"']++(safetyParse subsystemIn)++[':']++(safetyParse cgroupIn)++['"']
 let safeTaskId = ['"']++(safetyParse taskIdIn)++['"'] 
 exitCode <- liftIO $ system ("sudo cgclassify -g "++safeCgroupLocation++" "++safeTaskId)
 return ("Task " ++ taskIdIn ++ " classified into " ++ safeCgroupLocation ++ " with exit code "++(show exitCode))
