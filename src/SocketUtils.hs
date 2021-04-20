module SocketUtils where

import Network.Socket

resolveAddress :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
resolveAddress host port = do
        infos <- getAddrInfo (Just hints) host port
        return $ head infos
    where
        hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}