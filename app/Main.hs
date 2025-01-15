module Main (main) where

import Types
import Client
import Server
import Control.Concurrent (forkIO, newMVar)
import Control.Monad (replicateM_)

main :: IO ()
main = do
    queue <- newMVar [] -- Initialize an empty request queue
    _ <- forkIO $ processRequests queue handleRequest -- Start the server thread
    replicateM_ 10 $ \i -> forkIO $ client i queue -- Start 10 client threads

-- | Simulate handling a request and creating a response.
handleRequest :: Request -> IO Response
handleRequest req = do
    timestamp <- getCurrentTime
    return $ Response timestamp ("Response to: " ++ reqContent req)
