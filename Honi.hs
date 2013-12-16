{-# LANGUAGE ForeignFunctionInterface #-}

module Honi
  (ApiVersion
  , initialize, shutdown
  , getDeviceList
  )
where

import Control.Applicative
import Foreign
import Foreign.C

import Honi.Types

type OniStatus = CInt

toStatus :: OniStatus -> Status
toStatus = toEnum . fromIntegral

foreign import ccall unsafe "OniCAPI.h oniInitialize"
  oniInitialize :: CInt -> IO OniStatus

type ApiVersion = Int

initialize :: ApiVersion -> IO Status
initialize version
  = toStatus <$> oniInitialize (fromIntegral version)

foreign import ccall unsafe "OniCAPI.h oniShutdown"
  shutdown :: IO ()

foreign import ccall unsafe "OniCAPI.h oniGetDeviceList"
  oniGetDeviceList :: Ptr (Ptr DeviceInfo) -> Ptr CInt -> IO OniStatus

getDeviceList :: IO (Either Status [ DeviceInfo ])
getDeviceList = alloca $ \listPtr -> alloca $ \numPtr -> do
  r <- toStatus <$> oniGetDeviceList listPtr numPtr
  case r of
    StatusOK -> do
      num <- peek numPtr
      list <- peek listPtr
      deviceList <- peekArray (fromIntegral num) list
      free list
      return $ Right deviceList
    err -> return $ Left err