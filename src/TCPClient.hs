module TCPClient (runTCPClient, ConnHandler) where

import Network.Socket (socket, close, withSocketsDo, connect,
    AddrInfo(addrFamily, addrSocketType, addrProtocol, addrAddress),
    Socket, HostName, ServiceName)

import Control.Exception (bracket)

import SocketUtils (resolveAddress)

-- |Type of function to be called when a 'Socket' establishes a new connection
type ConnHandler = (Socket -> AddrInfo -> IO ())

-- |Starts a TCP client that attempts to connect to the given host and port. 'client' is called if the connection is successful
runTCPClient :: HostName -> ServiceName -> ConnHandler -> IO ()
runTCPClient host port client = withSocketsDo $ do
    address <- resolveAddress (Just host) (Just port)
    bracket (connectTo address) close (`client` address)

connectTo :: AddrInfo -> IO Socket
connectTo address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    connect sock (addrAddress address)
    return sock