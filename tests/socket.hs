{-# Language DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import qualified Data.ByteString as B

import Network.Game.Socket

data Cmd =
    Client
  | Server
  deriving (Show, Eq, Data, Typeable)

client :: System.Console.CmdArgs.Mode Cmd
client = mode $ Client
  &= text "socket client"

server :: System.Console.CmdArgs.Mode Cmd
server = mode $ Server
  &= text "socket server"

main :: IO ()
main = do
  cmd <- cmdArgs "minimal socket client and server" [client, server]
  processCmd cmd

processCmd Client = do
  msock <- socket 44004 1024

  when (isNothing msock) $ do
    putStrLn "Can't create udp socket."
    exitWith $ ExitFailure 1
  let Just sock = msock

  forever $ do
    _ <- send sock (127,0,0,1) 44005 "hello!"
    threadDelay 1000000 -- 1 second

processCmd Server = do
  msock <- socket 44005 1024

  when (isNothing msock) $ do
    putStrLn "Can't create udp socket."
    exitWith $ ExitFailure 1
  let Just sock = msock

  forever $ do
    x <- receive sock
    case x of
      Nothing -> return ()
      Just (bs, sender) -> do
        B.putStr bs
        putStrLn $ " from " ++ show sender
        threadDelay 1000000 -- 1 second

forever :: IO a -> IO ()
forever action = action >> forever action

