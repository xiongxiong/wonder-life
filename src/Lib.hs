module Lib
( 
    printTask,
    writeTask,
    readTask
) where

import RIO
import Data.Basic (newTask, createTask)
import Data.Store (savePlan, loadPlan)
import System.IO (print)

printTask :: IO ()
printTask = do
    task <- newTask "hello" "world"
    print task

writeTask :: IO ()
writeTask = do
    task <- newTask "hello" "world"
    let plan = createTask [] task
    savePlan plan "plan.txt"

readTask :: IO ()
readTask = do
    plan <- loadPlan "plan.txt"
    case plan of
        Nothing -> print ("nothing" :: String)
        (Just p) -> print p

