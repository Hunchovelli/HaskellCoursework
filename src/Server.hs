module Server where

import Types
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Time (getCurrentTime)
import System.IO (appendFile)

-- | Add a request to the queue.
addRequest :: RequestQueue -> Request -> IO ()
addRequest queue req = modifyMVar_ queue $ \requests -> return (requests ++ [req])

-- | Process requests from the queue.
processRequests :: RequestQueue -> (Request -> IO Response) -> IO ()
processRequests queue handler = forever $ do
    requests <- readMVar queue
    case requests of
        [] -> return () -- Do nothing if the queue is empty
        (req:rest) -> do
            modifyMVar_ queue $ \_ -> return rest
            response <- handler req
            putStrLn $ "Processed: " ++ show response

logRequestResponse :: Request -> Response -> IO ()
logRequestResponse req res = do
    appendFile "requests.log" (show req ++ "\n" ++ show res ++ "\n")