module TCPServer where

import Network.Socket (SockAddr, Socket, HostName, ServiceName, withSocketsDo,
    AddrInfo (addrAddress, addrProtocol, addrFamily, addrSocketType),
    SocketOption (ReuseAddr), setSocketOption, withFdSocket, accept, bind, listen,
    setCloseOnExecIfNeeded, close, gracefulClose, socket)

import Control.Exception (bracket)
import Control.Concurrent (forkFinally)
import Control.Monad (forever)

import SocketUtils (resolveAddress)

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> SockAddr -> IO ()) -> IO ()
runTCPServer host port connHandler = withSocketsDo $ do
    address <- resolveAddress host (Just port)
    bracket (startListen address) close (connAccepter connHandler) --listen to the given address and close the connection when the listen function exits

connAccepter :: (Socket -> SockAddr -> IO ()) -> Socket -> IO ()
connAccepter handlerFunc sock = forever $ do
    (connection, address) <- accept sock --accept all incoming connections
    forkFinally (handlerFunc connection address) (\_ -> gracefulClose connection 5000)

startListen :: AddrInfo -> IO Socket
startListen address = do --create a socket that is listening on the given address
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (addrAddress address)
    listen sock 1024
    return sock