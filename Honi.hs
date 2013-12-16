{-# LANGUAGE ForeignFunctionInterface #-}

module Honi
  (ApiVersion
  , initialize, shutdown
  , getDeviceList
  , deviceOpen, deviceOpenInfo, deviceClose, deviceGetSensorInfo
  )
where

import Control.Applicative
import qualified Data.ByteString as BS
import Foreign
import Foreign.C

import Honi.Types

type OniStatus = CInt
type Oni a = IO (Either Status a)

foreign import ccall unsafe "OniCAPI.h oniInitialize"
  oniInitialize :: CInt -> IO OniStatus

type ApiVersion = Int

initialize :: ApiVersion -> IO Status
initialize version
  = fromCInt <$> oniInitialize (fromIntegral version)

foreign import ccall unsafe "OniCAPI.h oniShutdown"
  shutdown :: IO ()

foreign import ccall unsafe "OniCAPI.h oniGetDeviceList"
  oniGetDeviceList :: Ptr (Ptr DeviceInfo) -> Ptr CInt -> IO OniStatus

foreign import ccall unsafe "OniCAPI.h oniReleaseDeviceList"
  oniReleaseDeviceList :: Ptr DeviceInfo -> IO OniStatus

getDeviceList :: Oni [ DeviceInfo ]
getDeviceList = alloca $ \listPtr -> alloca $ \numPtr ->
  whenOK (oniGetDeviceList listPtr numPtr) $ do
    num <- peek numPtr
    list <- peek listPtr
    deviceList <- peekArray (fromIntegral num) list
    oniReleaseDeviceList list
    return $ Right deviceList

whenOK :: IO OniStatus -> Oni a -> Oni a
whenOK oni ok = do
  r <- oni
  case fromCInt r of
    StatusOK -> ok
    err -> return $ Left err

foreign import ccall unsafe "OniCAPI.h oniDeviceOpen"
  oniDeviceOpen :: CString -> Ptr OpaquePtr -> IO OniStatus

deviceOpen :: BS.ByteString -> Oni DeviceHandle
deviceOpen uri
  = BS.useAsCString uri $ \uriPtr ->
      alloca $ \handlePtr ->
        whenOK (oniDeviceOpen uriPtr handlePtr)
          ((Right . DeviceHandle) <$> peek handlePtr)

deviceOpenInfo :: DeviceInfo -> Oni DeviceHandle
deviceOpenInfo = deviceOpen . uri

foreign import ccall unsafe "OniCAPI.h oniDeviceClose"
  oniDeviceClose :: OpaquePtr -> IO OniStatus

deviceClose :: DeviceHandle -> IO Status
deviceClose (DeviceHandle p) = fromCInt <$> oniDeviceClose p

foreign import ccall unsafe "OniCAPI.h oniDeviceGetSensorInfo"
  oniDeviceGetSensorInfo :: OpaquePtr -> CInt -> IO (Ptr SensorInfo)

deviceGetSensorInfo :: DeviceHandle -> SensorType -> IO (Maybe SensorInfo)
deviceGetSensorInfo (DeviceHandle p) sensorType = do
  sip <- oniDeviceGetSensorInfo p (toCInt sensorType)
  if sip == nullPtr
    then return Nothing
    else do
      sensorInfo <- peek sip
      free sip
      return $ Just sensorInfo
