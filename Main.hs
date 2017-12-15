{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment (getArgs)
import Argon
import qualified Pipes.Prelude as P
import Pipes
import System.IO.Silently
import Pipes.Safe (runSafeT)
import System.Process
import System.Directory (doesDirectoryExist)
import Control.Distributed.Process
import Data.string.Utils
import Control.Distributed.Process.Closure
import Control.Monad (forever, forM_)
import Data.List
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Data.List.Split

--getting the processid,nodeid and the string variable
wor_anal_comp :: (proc_id, node_id, string) -> Process ()
--Sending the masters,workerid and the git url
wor_anal_comp (master, worker_id, git_url) = do
  liftIO ( putStrLn $ "To start the  worker : " ++ (show worker_id) ++ " with the parameter values: " ++ git_url)
  let repository_name = last $ splitOn "/" git_url
  git_repo_exist <- liftIO $ doesDirectoryExist ("/tmp/" ++ repository_name)
--Check if the reposiroty exists or not
  if not git_repo_exist then do
  --If the repository exists then clone it
    liftIO $ call_process "/usr/bin/git" ["clone", git_url, "/tmp/" ++ repository_name]
  else do
    liftIO $ putStrLn "Repository exists."
  let config = (Config 6 [] [] [] Colored)
  let source = allFiles ("/tmp/" ++ repository_name)
              >-> P.mapM (liftIO . analyze config)
              >-> P.map (filterResults config)
              >-> P.filter filterNulls
  liftIO $ putStrLn $ "To launch the git url to  analyse for " ++ git_url
  (output, _) <- liftIO $ capture $ runSafeT $ runEffect $ exportStream config source
  liftIO ( putStrLn $ "End of the worker : " ++ (show worker_id) ++ " with the parameter: " ++ git_url)
  send master $ (worker_id, git_url, output)


remotable ['wor_anal_comp]


my_remote_table :: RemoteTable
my_remote_table = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      backend_var <- initializeBackend host port my_remote_table
      startMaster backend_var (master backend_var)
    ["slave", host, port] -> do
      backend_var <- initializeBackend host port my_remote_table
      startSlave backend_var

master :: Backend -> [node_id] -> Process ()
master backend_var slaves = do
  liftIO . putStrLn $ "slaves_info: " ++ show slaves
  let repository = ["https://github.com/phadej/github", "https://github.com/JakeWheat/hssqlppp", "https://github.com/hspec/hspec", "https://github.com/yesodweb/Shelly.hs", "https://github.com/vincenthz/hs-tls", "https://github.com/Paczesiowa/hsenv", "https://github.com/kallisti-dev/hs-webdriver","https://github.com/mvoidex/hsdev"]
  responses <- feedSlavesAndGetResponses repository slaves [] []
  liftIO $ mapM (\(r,u) -> putStrLn $ "\n\n\n\n********************\n" ++ u ++ " :\n*****************************\n\n" ++  r) responses
  return ()
  -- terminate the slaves

feedSlavesAndGetResponses :: [string] -> [node_id] -> [node_id] -> [(string,string)] -> Process [(string,string)]
feedSlavesAndGetResponses [] freeSlaves [] responses = return responses
feedSlavesAndGetResponses repository freeSlaves busySlaves responses = do
  (restRepos, newBusySlaves, newFreeSlaves) <- feedSlaves repository freeSlaves []
  m <- expectTimeout 50000000 -- 1min max for each repo
  case m of
    Nothing            -> die "Master  has a fatal failure, so is exiting."
    Just (slave, git_url, resp) -> feedSlavesAndGetResponses restRepos (slave:newFreeSlaves) (delete slave (newBusySlaves ++ busySlaves)) ((resp,git_url):responses)

feedSlaves :: [string] -> [node_id] -> [node_id] -> Process ([string], [node_id], [node_id])
feedSlaves [] slaves newBusySlaves = return ([], newBusySlaves, slaves)
feedSlaves repository [] newBusySlaves = return (repository, newBusySlaves, [])
feedSlaves (repo:repository) (oneSlave:slaves) newBusySlaves = do
  masterPid <- getSelfPid
  _ <- spawn oneSlave $ $(mkClosure 'wor_anal_comp) (masterPid, oneSlave, repo :: string)
  feedSlaves repository slaves (oneSlave:newBusySlaves)
