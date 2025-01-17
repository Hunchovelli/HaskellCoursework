module Types where

-- | Imports UTCTime for the Data.Time module. Used to track when requests and responses are created.
import Data.Time (UTCTime)
-- | Imports MVar from Control.Concurrent. Used to create a mutable variable for the request queue.
import Control.Concurrent (MVar)


-- | Represents a struct for request to be processed by the server.
data Request = Request {
     -- | ID of the request (to uniquely identify it).
    requestId :: Int,
    -- | Timestamp of the request
    timestampRequest :: UTCTime,
    -- | Content of the request 
    contentRequest   :: String
-- | The request type is an instance of the Show typeclass, which allows it to be converted to a string for logging and debugging purposes.
} deriving (Show)

-- | Represents a struct for the response to a request.
data Response = Response {
     -- | ID of the response (matching the request ID).
    responseId :: Int,
    -- | Timestamp of the response
    timestampResponse :: UTCTime, 
    -- | Content of the response
    contentResponse   :: String
-- | The response type is an instance of the Show typeclass, which allows it to be converted to a string for logging and debugging purposes.
} deriving (Show)

-- | Represents a log entry that tracks a request, its response, and the latency.
data LogEntry = LogEntry {
    -- | The request that was sent.
    logRequest :: Request,
    -- | The response that was generated.
    logResponse :: Response,
    -- | Latency between the request and response (in seconds).
    latency :: Float
} deriving (Show)

-- | declare a type alias for the queue of requests
type RequestQueue = MVar[Request]

-- | Type alias for a mutable log that tracks request-response pairs and latencies.
type Log = MVar[LogEntry]