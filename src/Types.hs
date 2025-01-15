module Types where

-- | Imports UTCTime for the Data.Time module. Used to track when requests and responses are created.
import Data.Time (UTCTime)

-- | Represents a struct for request to be processed by the server.
data Request = Request {
    -- | Timestamp of the request
    timestampRequest :: UTCTime,
    -- | Content of the request 
    contentRequest   :: String 
-- | The request type is an instance of the Show typeclass, which allows it to be converted to a string for logging and debugging purposes.
} deriving (Show)

-- | Represents a struct for the response to a request.
data Response = Response {
    -- | Timestamp of the response
    timestampResponse :: UTCTime, 
    -- | Content of the response
    contentResponse   :: String 
-- | The response type is an instance of the Show typeclass, which allows it to be converted to a string for logging and debugging purposes.
} deriving (Show)

-- | declare a type for the queue of requests
type RequestQueue = MVar [Request]
