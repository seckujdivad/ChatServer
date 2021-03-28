module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Control.Monad (unless, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, tryReadTChan, writeTChan, newBroadcastTChan)

import qualified Data.ByteString
import qualified Data.ByteString.Char8

import TCPServer (runTCPServer)

type ServerInterface = (TChan String, TChan String) --mainloop input, mainloop output

main :: IO ()
main = do
    mainloopIn <- atomically newTChan
    mainloopOut <- atomically newBroadcastTChan
    let interface = (mainloopIn, mainloopOut)
    forkIO (serverMainloop interface)
    runTCPServer Nothing "3000" interface connHandler

connHandler :: Socket -> SockAddr -> ServerInterface -> IO ()
connHandler connection address (mainloopIn, mainloopOut) = do
    mainloopOutDuplicated <- atomically $ dupTChan mainloopOut
    forkIO (connSender connection address mainloopOutDuplicated)
    forever $ do
        message <- recv connection 1024
        atomically $ writeTChan mainloopIn (Data.ByteString.Char8.unpack message)

connSender :: Socket -> SockAddr -> TChan String -> IO ()
connSender connection address mainloopOut = forever $ do
    toSendMaybe <- atomically $ tryReadTChan mainloopOut
    case toSendMaybe of
        Just toSend -> sendAll connection (Data.ByteString.Char8.pack toSend)
        Nothing -> return ()

serverMainloop :: ServerInterface -> IO ()
serverMainloop (mainloopIn, mainloopOut) =  forever $ do
    message <- atomically $ tryReadTChan mainloopIn
    case message of
        Just messageContents -> do
           atomically $ writeTChan mainloopOut messageContents
        Nothing -> return ()