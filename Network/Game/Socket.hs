{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
-- | Simple UDP networking with non-blocking receive function. This is needed
-- because network and network-bytestring implement a blocking recv.
module Network.Game.Socket
  ( Socket
  , socket, close , send, receive, sendBytes, receiveBytes
  ) where

import Foreign
import Foreign.C.Types
import Data.ByteString
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Serialize

data Socket = Socket (Ptr Socket_) Int -- buffer size for the receive function

data Socket_

foreign import ccall unsafe "socket_open"
  c_socket_open :: CUShort -> IO (Ptr Socket_)

socket :: Int -> Int -> IO (Maybe Socket)
socket port nbytes = do
  s <- c_socket_open (fromIntegral port)
  if s == nullPtr
    then return Nothing
    else return . Just $ Socket s nbytes

foreign import ccall unsafe "socket_close"
  c_socket_close :: Ptr Socket_ -> IO ()

close :: Socket -> IO ()
close (Socket sock _) =
  c_socket_close sock

foreign import ccall unsafe "socket_send"
  c_socket_send :: Ptr Socket_ -> CUChar -> CUChar -> CUChar -> CUChar
  -> CUShort -> Ptr CChar -> CInt -> IO CInt

sendBytes :: Socket -> (Int,Int,Int,Int) -> Int -> ByteString -> IO Bool
sendBytes (Socket sock _) (a,b,c,d) port bs = do
  r <- unsafeUseAsCStringLen bs $ \(ptr, len) ->
    c_socket_send sock (fromIntegral a) (fromIntegral b) (fromIntegral c)
      (fromIntegral d) (fromIntegral port) ptr (fromIntegral len)
  return $ r == 1

send :: Serialize a => Socket -> (Int,Int,Int,Int) -> Int -> a -> IO Bool
send sock addr port a = sendBytes sock addr port $ encode a

foreign import ccall unsafe "socket_receive"
  c_socket_receive :: Ptr Socket_ -> Ptr CUChar -> Ptr CUChar -> Ptr CUChar
  -> Ptr CUChar -> Ptr CUShort -> Ptr CChar -> CInt -> IO CInt

receiveBytes :: Socket -> Int
  -> IO (Maybe (ByteString, ((Int,Int,Int,Int), Int)))
receiveBytes (Socket sock _) nbytes =
  alloca $ \a ->
  alloca $ \b ->
  alloca $ \c ->
  alloca $ \d ->
  alloca $ \port ->
  allocaBytes nbytes $ \ptr -> do
    len <- c_socket_receive sock a b c d port ptr (fromIntegral nbytes)
    if len == 0
      then return Nothing
      else do
        let f = fromIntegral
        bs <- packCStringLen (ptr, fromIntegral len)
        a' <- peek a
        b' <- peek b
        c' <- peek c
        d' <- peek d
        port' <- peek port
        return $ Just (bs, ((f a', f b', f c', f d'), fromIntegral port'))

receive :: Serialize a => Socket
  -> IO (Maybe (a, ((Int,Int,Int,Int), Int)))
receive s@(Socket _ nbytes) = do
  x <- receiveBytes s nbytes
  case x of
    Nothing -> return Nothing
    Just (bs, addrAndPort) ->
      case decode bs of
        Left _ -> return Nothing
        Right a -> return $ Just (a, addrAndPort)

