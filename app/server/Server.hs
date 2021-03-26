module Server where

import SocketUtils (resolveAddress)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Control.Monad
import qualified Control.Exception
import Control.Concurrent (forkFinally)

import qualified Data.ByteString
import qualified Data.ByteString.Char8

main :: IO ()
main = runTCPServer Nothing "3000" connHandler

type ConnHandler = Socket -> SockAddr -> IO ()

connHandler :: ConnHandler
connHandler connection address = do
    message <- recv connection 1024

    unless (Data.ByteString.null message) $ do
        putStr "Message: "
        putStrLn $ Data.ByteString.Char8.unpack message
        sendAll connection message
    
    unless (message == Data.ByteString.Char8.pack "") $ do
        connHandler connection address

connAccepter :: ConnHandler -> Socket -> IO ()
connAccepter handlerFunc sock = forever $ do
    (connection, address) <- accept sock
    forkFinally (handlerFunc connection address) (\_ -> gracefulClose connection 5000)

runTCPServer :: Maybe HostName -> ServiceName -> ConnHandler -> IO ()
runTCPServer host port server = withSocketsDo $ do
        address <- resolveAddress host (Just port)
        Control.Exception.bracket (startListen address) close (connAccepter server)

startListen :: AddrInfo -> IO Socket
startListen address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (addrAddress address)
    listen sock 1024
    return sock