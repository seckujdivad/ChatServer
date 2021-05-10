module Client where

import Data.ByteString.Char8 (pack, putStrLn)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad (unless, forever)
import Control.Concurrent (forkIO)

import TCPClient (runTCPClient, ConnHandler)

main :: IO ()
main = runTCPClient "127.0.0.1" "4321" connHandler

connHandler :: ConnHandler
connHandler connection address = do
    forkIO (connReceiver connection address)
    forever $ do
        toSend <- getLine
        sendAll connection (pack toSend)

connReceiver :: ConnHandler
connReceiver connection address = forever $ do
    message <- recv connection 1024
    putStr "Received: "
    Data.ByteString.Char8.putStrLn message