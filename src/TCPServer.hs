module TCPServer where

import Network.Socket
import qualified Control.Exception
import Control.Concurrent (forkIO, forkFinally)
import Control.Monad (forever)

import SocketUtils (resolveAddress)

runTCPServer :: Maybe HostName -> ServiceName -> a -> (Socket -> SockAddr -> a -> IO ()) -> IO ()
runTCPServer host port commData connHandler = withSocketsDo $ do
    address <- resolveAddress host (Just port)
    Control.Exception.bracket (startListen address) close (connAccepter connHandler commData)

connAccepter :: (Socket -> SockAddr -> a -> IO ()) -> a -> Socket -> IO ()
connAccepter handlerFunc handlerData sock = forever $ do
    (connection, address) <- accept sock
    forkFinally (handlerFunc connection address handlerData) (\_ -> gracefulClose connection 5000)

startListen :: AddrInfo -> IO Socket
startListen address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (addrAddress address)
    listen sock 1024
    return sock