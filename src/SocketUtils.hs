module SocketUtils (resolveAddress) where

import Network.Socket (ServiceName, SocketType(Stream), AddrInfo(addrFlags, addrSocketType),
    HostName, defaultHints, getAddrInfo, AddrInfoFlag(AI_PASSIVE))

-- |Constructs an 'AddrInfo' for connecting a 'Socket'
resolveAddress :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
resolveAddress host port = do
        infos <- getAddrInfo (Just hints) host port
        return (head infos)
    where
        hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}