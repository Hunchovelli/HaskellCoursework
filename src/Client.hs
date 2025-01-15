module Client where

import Types
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Time (getCurrentTime)

-- | Simulate a client sending requests.
client :: Int -> RequestQueue -> IO ()
client clientId queue = do
    delay <- randomRIO (1000000, 5000000) -- Random delay between 1-5 seconds
    threadDelay delay
    timestamp <- getCurrentTime
    let req = Request timestamp ("Client " ++ show clientId ++ " Request")
    addRequest queue req
    putStrLn $ "Client " ++ show clientId ++ " sent a request."
    client clientId queue -- Continue sending requests
