module TCPClient where

import Network.Socket (socket, close, withSocketsDo, connect,
    AddrInfo(addrFamily, addrSocketType, addrProtocol, addrAddress),
    Socket, HostName, ServiceName)

import Control.Exception (bracket)

import SocketUtils (resolveAddress)

type ConnHandler = (Socket -> AddrInfo -> IO ())

runTCPClient :: HostName -> ServiceName -> ConnHandler -> IO ()
runTCPClient host port client = withSocketsDo $ do
    address <- resolveAddress (Just host) (Just port)
    bracket (connectTo address) close (`client` address)

connectTo :: AddrInfo -> IO Socket
connectTo address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    connect sock (addrAddress address)
    return sock