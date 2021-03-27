module Server where

import SocketUtils (resolveAddress)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Control.Monad
import Control.Monad.STM (atomically)
import qualified Control.Exception
import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan, dupTChan, readTChan, tryReadTChan, writeTChan, isEmptyTChan)

import qualified Data.ByteString
import qualified Data.ByteString.Char8

main :: IO ()
main = runTCPServer Nothing "3000" connHandler serverMainloop

type ConnHandler = Socket -> SockAddr -> ServerInterface -> IO ()
type ServerMainloop = ServerInterface -> IO ()
type ServerInterface = (TChan String, TChan String) --first channel is written to by handlers, second by the mainloop

connHandler :: ConnHandler
connHandler connection address (outputChannel, inputChannel) = do
    message <- recv connection 1024

    unless (Data.ByteString.null message) $ do
        putStr "Message: "
        let strMessage = Data.ByteString.Char8.unpack message
        putStrLn strMessage
        atomically (writeTChan outputChannel strMessage)
    
    connHandlerChannelReader inputChannel connection
    
    connHandler connection address (outputChannel, inputChannel)

connHandlerChannelReader :: TChan String -> Socket -> IO ()
connHandlerChannelReader inputChannel connection = do
    chanIsEmpty <- atomically $ isEmptyTChan inputChannel
    unless chanIsEmpty $ do
        message <- atomically $ readTChan inputChannel
        sendAll connection (Data.ByteString.Char8.pack message)
        connHandlerChannelReader inputChannel connection

serverMainloop :: ServerMainloop
serverMainloop (inputChannel, outputChannel) = do
    forever $ do
        toBroadcast <- atomically $ readTChan inputChannel
        putStr "Rebroadcasting: "
        putStrLn toBroadcast
        atomically (writeTChan outputChannel toBroadcast)

connAccepter :: ConnHandler -> ServerInterface -> Socket -> IO ()
connAccepter handlerFunc (mainloopInChan, mainloopOutChan) sock = forever $ do
    (connection, address) <- accept sock
    connHandlerInChan <- atomically (dupTChan mainloopOutChan)

    forkFinally (handlerFunc connection address (mainloopInChan, connHandlerInChan)) (\_ -> gracefulClose connection 5000)

runTCPServer :: Maybe HostName -> ServiceName -> ConnHandler -> ServerMainloop -> IO ()
runTCPServer host port server mainloop = withSocketsDo $ do
    address <- resolveAddress host (Just port)
    mainloopInChan <- atomically newTChan
    mainloopOutChan <- atomically newTChan
    forkIO (serverMainloop (mainloopInChan, mainloopOutChan))
    Control.Exception.bracket (startListen address) close (connAccepter server (mainloopInChan, mainloopOutChan))

startListen :: AddrInfo -> IO Socket
startListen address = do
    sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (addrAddress address)
    listen sock 1024
    return sock