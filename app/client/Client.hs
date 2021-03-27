module Client where

import SocketUtils (resolveAddress)

import qualified Control.Exception
import qualified Data.ByteString.Char8
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad (unless, forever)
import Control.Concurrent (forkIO)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" connHandler

type ConnHandler = (Socket -> AddrInfo -> IO ())

connHandler :: ConnHandler
connHandler connection address = do
    forkIO (connReceiver connection address)
    forever $ do
        toSend <- getLine
        sendAll connection (Data.ByteString.Char8.pack toSend)

connReceiver :: ConnHandler
connReceiver connection address = forever $ do
    message <- recv connection 1024
    putStr "Received: "
    Data.ByteString.Char8.putStrLn message

runTCPClient :: HostName -> ServiceName -> ConnHandler -> IO ()
runTCPClient host port client = withSocketsDo $ do
    address <- resolveAddress (Just host) (Just port)
    Control.Exception.bracket (connectTo address) close (`client` address)

connectTo :: AddrInfo -> IO Socket
connectTo address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    connect sock (addrAddress address)
    return sock