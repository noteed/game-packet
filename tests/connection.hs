{-# Language DeriveDataTypeable, OverloadedStrings #-}
module Main where

import System.Console.CmdArgs
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import qualified Data.ByteString as B

import Network.Game.Connection hiding (client, server)
import qualified Network.Game.Connection as Con

data Cmd =
    Client
  | Server
  deriving (Show, Eq, Data, Typeable)

client :: System.Console.CmdArgs.Mode Cmd
client = mode $ Client
  &= text "client connection"

server :: System.Console.CmdArgs.Mode Cmd
server = mode $ Server
  &= text "server connection"

main :: IO ()
main = do
  cmd <- cmdArgs "minimal client and server connection" [client, server]
  processCmd cmd

processCmd Client = mainClient

processCmd Server = mainServer

mainServer :: IO ()
mainServer = do
  let prot = B.pack [0x99, 0x88, 0x77, 0x66]
  msv <- Con.server prot 10.0 30000
  when (isNothing msv) $ do
    putStrLn "Can't create server socket."
    exitWith $ ExitFailure 1

  let Just sv = msv
  forever $ do
    c <- isConnectedServer sv
    let payload = "server packet" :: B.ByteString
    when c $ sendServer sv payload >> return ()

    receivePackets sv

    updateServer sv 0.5
    threadDelay 500000

receivePackets sv = do
  mbs <- receiveServer sv
  unless (isNothing mbs) $ do
    let Just bs = mbs
    putStr "received client packet: "
    B.putStrLn bs
    receivePackets sv

mainClient :: IO ()
mainClient = do
  let prot = B.pack [0x99, 0x88, 0x77, 0x66]
  mcl <- Con.client ((127,0,0,1), 30000)  prot 10.0 30001
  when (isNothing mcl) $ do
    putStrLn "Can't create client socket."
    exitWith $ ExitFailure 1

  let Just cl = mcl
  unconnected cl

unconnected cl = do
  sendAndReceive cl
  c <- getStateClient cl
  case c of
    ClientConnecting ->
      unconnected cl
    ClientFailed -> do
      putStrLn "client coudn't connect to the server"
    ClientConnected -> do
      putStrLn "client connected to the server"
      connected cl
    ClientDisconnected -> error "can't happen"
    ClientClosed -> error "can't happen"

connected cl = do
  sendAndReceive cl
  c <- getStateClient cl
  if c == ClientConnected
    then connected cl
    else putStrLn "lost connection"

sendAndReceive cl = do
  let payload = "client packet" :: B.ByteString
  sendClient cl payload
  receivePacketsClient cl
  updateClient cl 0.5
  threadDelay 500000

receivePacketsClient cl = do
  mbs <- receiveClient cl
  unless (isNothing mbs) $ do
    let Just bs = mbs :: Maybe B.ByteString
    putStr "received server packet: "
    B.putStrLn bs
    receivePacketsClient cl

forever :: IO a -> IO ()
forever a = a >> forever a 

