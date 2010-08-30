{-# Language RecordWildCards, OverloadedStrings #-}
-- 
-- Based on http://gafferongames.wordpress.com/networking-for-game-programmers/virtual-connection-over-udp/
--
-- The Connection module provides two different kinds of connections:
-- Server and Client. Connections are built above the Socket layer.
-- They add the notion of connecting/connected/disconnected.
-- TODO the ServerState and ClientState should be cleaned.
module Network.Game.Connection
  ( Server, Client
  , ServerState(..), ClientState(..)
  , server, client
  , receiveServer, receiveClient
  , sendServer, sendClient
  , updateServer, updateClient
  , stopServer, stopClient
  , getStateClient
  , isConnectedServer
  ) where

import qualified Data.ByteString as B
import Data.ByteString.Char8 () -- for the IsString instance
import Data.IORef
import Control.Monad (when)
import Data.Serialize

import qualified Network.Game.Socket as Socket

-- some magic number to prefix each payload

type Address = ((Int,Int,Int,Int),Int)

data Server = Server
  { svProtocol :: B.ByteString
  -- ^ e.g. a 32 bits magic value.
  , svTimeout :: Float
  , svSocket :: Socket.Socket
  , svState :: IORef ServerState
  , svElapsed :: IORef Float
  }

data Client = Client
  { clProtocol :: B.ByteString
  -- ^ e.g. a 32 bits magic value.
  , clTimeout :: Float
  , clSocket :: Socket.Socket
  , clState :: IORef ClientState
  , clElapsed :: IORef Float
  , clAddress :: Address
  }

data ServerState =
    ServerListening
  | ServerFailed
  | ServerConnected Address
  | ServerDisconnected Address
  | ServerClosed
  deriving (Show, Eq)

data ClientState =
    ClientConnecting
  | ClientFailed
  | ClientConnected
  | ClientDisconnected
  | ClientClosed
  deriving (Show, Eq)

getStateClient :: Client -> IO ClientState
getStateClient Client{..} = readIORef clState

isConnectedServer :: Server -> IO Bool
isConnectedServer Server{..} = do
  s <- readIORef svState
  case s of
    ServerConnected _ -> return True
    _ -> return False

timeoutServer :: ServerState -> ServerState
timeoutServer s = case s of
  ServerListening -> ServerListening -- ServerFailed
  ServerFailed -> ServerFailed
  ServerConnected _ -> ServerListening -- ServerDisconnected a
  ServerDisconnected a -> ServerDisconnected a
  ServerClosed -> ServerClosed

timeoutClient :: ClientState -> ClientState
timeoutClient s = case s of
  ClientConnecting -> ClientFailed
  ClientFailed -> ClientFailed
  ClientConnected -> ClientDisconnected
  ClientDisconnected -> ClientDisconnected
  ClientClosed -> ClientClosed

server :: B.ByteString -> Float -> Int -> IO (Maybe Server)
server protocol timeout port = do
  msock <- Socket.socket port 1024
  case msock of
    Nothing -> return Nothing
    Just sock -> do
      s <- newIORef ServerListening
      e <- newIORef 0
      return . Just $ Server protocol timeout sock s e

client :: Address -> B.ByteString -> Float -> Int -> IO (Maybe Client)
client addr protocol timeout port = do
  msock <- Socket.socket port 1024
  case msock of
    Nothing -> return Nothing
    Just sock -> do
      s <- newIORef ClientConnecting
      e <- newIORef 0
      return . Just $ Client protocol timeout sock s e addr

stopServer :: Server -> IO ()
stopServer Server{..} = do
  Socket.close svSocket
  writeIORef svState ServerClosed

stopClient :: Client -> IO ()
stopClient Client{..} = do
  Socket.close clSocket
  writeIORef clState ClientClosed

updateServer :: Server -> Float -> IO ()
updateServer Server{..} dt = do
  modifyIORef svElapsed (+ dt)
  elapsed <- readIORef svElapsed
  when (elapsed > svTimeout) $
    modifyIORef svState timeoutServer

updateClient :: Client -> Float -> IO ()
updateClient Client{..} dt = do
  modifyIORef clElapsed (+ dt)
  elapsed <- readIORef clElapsed
  when (elapsed > clTimeout) $
    modifyIORef clState timeoutClient

sendServer :: Serialize a => Server -> a -> IO Bool
sendServer Server{..} a = do
  ServerConnected (addr, port) <- readIORef svState
  writeIORef svState $ ServerConnected (addr, port)
  Socket.send svSocket addr port (runPut $ putByteString svProtocol >> put a)

sendClient :: Serialize a => Client -> a -> IO Bool
sendClient Client{..} a = Socket.send clSocket
  (fst clAddress) (snd clAddress) (runPut $ putByteString clProtocol >> put a)

decodePacket :: Serialize a => B.ByteString -> B.ByteString -> Either String a
decodePacket prot bs = flip runGet bs $ do
  prot' <- getBytes $ B.length prot
  if prot /= prot'
    then fail "protocols do not match"
    else get

receiveServer :: Serialize a => Server -> IO (Maybe a)
receiveServer Server{..} = do
  x <- Socket.receive svSocket
  case x of
    Nothing -> return Nothing
    Just (bs_, sender) -> do
      case decodePacket svProtocol bs_ of
        Left _ -> return Nothing
        Right a -> do
          s <- readIORef svState
          when (s == ServerListening) $
            writeIORef svState (ServerConnected sender)

          -- this is a caller error if the state is not Connected
          ServerConnected addr <- readIORef svState
          if sender /= addr
            then return Nothing
            else do
              writeIORef svElapsed 0
              return $ Just a

receiveClient :: Serialize a => Client -> IO (Maybe a)
receiveClient Client{..} = do
  x <- Socket.receive clSocket
  case x of
    Nothing -> return Nothing
    Just (bs_, sender) ->
      case decodePacket clProtocol bs_ of
        Left _ -> return Nothing
        Right a -> do
          if sender /= clAddress
            then return Nothing
            else do
              -- connecting client only
              writeIORef clState ClientConnected
              -- connected client
              writeIORef clElapsed 0
              return $ Just a

