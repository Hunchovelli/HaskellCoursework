module Server where

-- | Imports the Types module
import Types
-- | Imports the Control.Concurrent module
import Control.Concurrent (MVar, modifyMVar_, readMVar, threadDelay)
-- | Imports the Control.Monad module
import Control.Monad (forever)
-- | Imports the Data.Time module
import Data.Time (getCurrentTime, diffUTCTime)
-- | Imports the System.IO module
import System.IO (appendFile)
-- | Imports the System.Exit module
import Data.Time (UTCTime)
-- | Imports the System.Exit module
import System.Exit (exitSuccess)
-- | Imports the Data.List module
import Data.List (sortOn)


-- | Add a request to the queue.
addRequest :: RequestQueue -> Request -> IO ()
-- | The addRequest function takes a RequestQueue (MVar) and request as arguments and adds the new request to the queue by using modifyMvar which updates the Mvar even if there is a value inside it.
addRequest queue req = modifyMVar_ queue $ \requests -> return (requests ++ [req])

-- | Process requests from the queue.
processRequests :: RequestQueue -> (Request -> IO Response) -> MVar Int -> Log -> IO ()
-- | The processRequests function takes a RequestQueue and a handler function as arguments. It processes requests from the queue by reading the requests from the queue and calling the handler function on each request. It then logs the request and response to a file.
processRequests queue handler requestCounter logVar = forever $ do -- | The forever function takes an IO action and repeats it indefinitely.
    -- | reads the current count
    count <- readMVar requestCounter
    -- | reads the current state of the MVar[log]
    log <- readMVar logVar
    -- | reads the current state of the queue
    if count >= 100 then do
        putStrLn "Client requests limit reached - 100 requests. Server shutting down."
        exitSuccess
        else do
            -- | reads the current state of the queue
            requests <- readMVar queue
            -- | coniditional statement to check if the requests queue is empty or full
            case requests of
                -- | Do nothing if the queue is empty
                [] -> return () 
                -- | If the queue is not empty, process the request
                (req:rest) -> do
                    -- | Modify the MVar to remove the first request within the queue that needs to be processed first and leave the rest of the requests in the queue.
                    modifyMVar_ queue $ \_ -> return rest
                    -- | Extracts the first request from the queue and processes it using the handler function, FIFO
                    response <- handler req log count 
                    -- | Outputs the response to the console
                    putStrLn $ "Processed: " ++ show response

-- | Process a single request and return a response
handler :: Request -> Log -> Int -> IO Response
handler req log count  = do
    -- | Get the current timestamp for the server response
    timestamp <- getCurrentTime
    -- | Calculate latency and converts it into a float from the difference between the request timestamp and the response timestamp
    let latency = realToFrac $ diffUTCTime timestamp (timestampRequest req) :: Float
    -- | Generate response content
    let contentResponse = "Processed: " ++ contentRequest req 
    -- | Create the response
    let response = Response count timestamp contentResponse
    -- | Log the request and response
    logRequestResponse log req response latency
    -- | Return the response
    return response
                    
 -- | Logs the request and response to a file.                   
logRequestResponse :: Log -> Request -> Response -> Float -> IO ()
-- | The logRequestResponse function takes a Request and Response as arguments and logs them to a file. 
logRequestResponse logVar req res lat = do
    -- | Create a new log entry
    let logEntry = LogEntry req res lat
    -- | Log the request with the updated response and latency.
    modifyMVar_ logVar $ \log -> return $ log ++ [logEntry]
    -- | Format a log string to be outputted to the requests.log file
    -- | It appends the request and response to a file named requests.log.
    appendFile "requests.log" (unlines ["Request: " ++ show req, "Response: " ++ show res, "Latency: " ++ show lat ++ " seconds", "-----------------------------------"])
   

-- | Wait until the request counter reaches 100
waitForCompletion :: MVar Int -> IO ()
waitForCompletion requestCounter = do
    count <- readMVar requestCounter
    if count < 100
        -- | Check every 0.1 seconds
        then threadDelay 100000 >> waitForCompletion requestCounter 
        else putStrLn "All requests processed. Exiting program..."

fastestResponse :: Log -> IO ()
fastestResponse logVar = do
    log <- readMVar logVar
    let fastestLatency = minimum $ map latency log
    putStrLn $ "Fastest response: " ++ show fastestLatency ++ " seconds"