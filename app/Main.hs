module Main (main) where
-- | Imports all the types from Types.hs
import Types
-- | Imports the Client.hs module
import Client
-- | Imports the Server.hs module
import Server
-- | Imports forkIO and newMVar from Control.Concurrent to create a new thread and an empty mVar.
import Control.Concurrent (MVar, forkIO, newMVar)
-- | Imports replicateM_ from Control.Monad to create 10 client threads.
import Control.Monad (replicateM_)
-- | Imports getCurrentTime from Data.Time to get the current time.
import Data.Time (getCurrentTime)

main :: IO ()
main = do
    -- | Create an empty mVar[request] for the FIFO queue
    queue <- newMVar [] 
    -- | Create an MVar to track the number of processed requests
    requestCounter <- newMVar 0
    -- | Create an empty MVar[log] for the log
    logVar <- newMVar []
    -- | Start the server thread
    _ <- forkIO $ processRequests queue handler requestCounter logVar
    -- | Start 10 client threads
    replicateM_ 10 (forkIO (client queue requestCounter logVar))
    -- | Wait until all requests are processed
    waitForCompletion requestCounter
    -- | Get the final log
    fastestResponse logVar
   
    




        
