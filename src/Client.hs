module Client where

-- | Imports all the types from Types.hs
import Types
-- | Imports addRequest function from Server.hs
import Server (addRequest)
-- | Imports delay feature to add delay between requests.
import Control.Concurrent (MVar, readMVar, threadDelay, modifyMVar_)
-- | Imports randomRIO from System.Random to generate random numbers.
import System.Random (randomRIO)
-- | Imports getCurrentTime from Data.Time to get the current time.
import Data.Time (getCurrentTime)

-- | Simulate a client sending requests.
client :: RequestQueue -> MVar Int -> Log -> IO ()
client queue requestCounter logVar = do
    count <- readMVar requestCounter
    -- | Generate a random delay between 1 and 5 seconds
    delay <- randomRIO (1000000, 5000000)
    -- | Pauses the thread for the specified delay geneated in previous line
    threadDelay delay
    -- | Gets the current time
    timestamp <- getCurrentTime
    -- | Creates a request with the current time and content.
    let req = Request count timestamp ("Request " ++ show count ++ " from client") 
    -- | Adds the request to the queue
    addRequest queue req
    -- | Log the request with a placeholder response (to be updated later when latency calculated).
    modifyMVar_ logVar $ \log -> return $ log ++ [LogEntry req (Response count timestamp "Pending") 0.0]
    -- Increment the counter
    modifyMVar_ requestCounter $ \c -> return (c + 1)
    -- | Outputs string to the console to indicate that a request was sent.
    putStrLn $ "Client sent a request."
    -- Recursively keep sending requests
    client queue requestCounter logVar
    
            
                
