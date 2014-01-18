module Util.CGroupUtil where

import Import
import Model.CGroupModel
import Data.List
import Data.Map()
import Data.Char(isDigit)
import Data.Text()

-- | This function ensures nothing can be injected into the calls
safetyParse :: String -> String
safetyParse [] = []
safetyParse (x:xs)
 | x `elem` ['0'..'9'] = x:safetyParse xs
 | x `elem` [':','/','.','-',',','='] = x:safetyParse xs
 | x `elem` ['A'..'z'] = x:safetyParse xs
 | otherwise = safetyParse xs

-- |Parse the subsystem component component from a subsystem:cgroup string
getSubSystem :: String -> String
getSubSystem [] = []
getSubSystem (x:xs)
 | x /= ':' = x : getSubSystem xs
 | otherwise = []

-- |Parse the cgroup component component from a subsystem:cgroup string
getCGroup :: String -> String
getCGroup [] = []
getCGroup (x:xs)
 | x == ':' = xs
 | otherwise = getCGroup xs

-- |Parse the root node of an indented list of process groups
parseProcessGroup :: [String] -> ProcessGroup
parseProcessGroup [] = error "No process groups found"
parseProcessGroup (x:xs)
 | otherwise = ProcessGroup nameWithoutPosition (parseProcessGroupChildren childrenLocal)
 where firstNumber = read ((head x):[]) :: Int
       childrenLocal = sourceChildren firstNumber xs
       nameWithoutPosition = tail x

-- |Parse the child nodes of an indented list of process groups
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

-- |Given a list of nodes filter all children of the specified parent position
sourceChildren :: Int -> [String] -> [String]
sourceChildren _ [] = []
sourceChildren parentPosition (x:xs)
 | firstNumber > parentPosition = x:(sourceChildren parentPosition xs)
 | otherwise = []
 where firstNumber = read ((head x):[]) :: Int

-- |Given a list of nodes filter the direct siblings of the specified parent position
sourceSiblings :: Int -> [String] -> [String]
sourceSiblings _ [] = []
sourceSiblings parentPosition (x:xs)
 | firstNumber > parentPosition = (sourceSiblings parentPosition xs)
 | otherwise = x:xs
 where firstNumber = read ((head x):[]) :: Int

-- |Parse the task id from a string prefixed with the task id
getTaskId :: String -> String
getTaskId [] = []
getTaskId (x:xs)
 | isDigit x = x : getTaskId xs
 | otherwise = []

-- |Parse the task description from a string prefixed with the task id
getTaskDescription :: String -> String
getTaskDescription [] = []
getTaskDescription (x:xs)
 | isDigit x = getTaskDescription xs
 | x == ' ' = getTaskDescription xs
 | otherwise = xs

-- |Convert indented positions to a numeric position
formatSourceLinePositions :: [String] -> [String]
formatSourceLinePositions [] = []
formatSourceLinePositions (x:xs) 
 | x == [] = []
 | otherwise = (show position ++ drop position x) : formatSourceLinePositions xs
 where position = countPrefix x

-- |Count the number of ' ' or '├' or '─' or '└' or '│' characters in a given string
countPrefix :: String -> Int
countPrefix [] = 0
countPrefix (x:xs) 
 | x == ' ' = 1 + countPrefix xs
 | x == '├' = 1 + countPrefix xs
 | x == '─' = 1 + countPrefix xs
 | x == '└' = 1 + countPrefix xs
 | x == '│' = 1 + countPrefix xs
 | otherwise = 0
