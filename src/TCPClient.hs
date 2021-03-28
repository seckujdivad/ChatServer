module TCPClient where

import qualified Control.Exception
import Network.Socket

import SocketUtils (resolveAddress)

type ConnHandler = (Socket -> AddrInfo -> IO ())

runTCPClient :: HostName -> ServiceName -> ConnHandler -> IO ()
runTCPClient host port client = withSocketsDo $ do
    address <- resolveAddress (Just host) (Just port)
    Control.Exception.bracket (connectTo address) close (`client` address)

connectTo :: AddrInfo -> IO Socket
connectTo address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    connect sock (addrAddress address)
    return sock