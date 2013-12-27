{-# LANGUAGE ForeignFunctionInterface #-}

-- | Binding for the OpenNI C API (OniCAPI.h).
--
-- Example use:
--
-- >import Honi
-- >import Honi.Types
-- >
-- >initialize 2
-- >Right (di:_) <- getDeviceList
-- >Right d      <- deviceOpenInfo di
-- >deviceGetSensorInfo d SensorDepth
--
-- This module does not use `error`.
-- All its potential errors can be caught as `HoniBug`.
module Honi
  ( ApiVersion
  , Oni
  -- * General
  , initialize, shutdown
  , getDeviceList
  , deviceOpen
  , deviceOpenInfo
  , deviceClose
  , deviceGetSensorInfo
  , deviceCreateStream

  -- * Streams
  , streamStart
  , streamStop
  )
where

import Control.Applicative
import qualified Data.ByteString as BS
import Foreign
import Foreign.C

import Honi.Types

-- Internal use only: for when we don't want to convert to a `Status`.
type OniStatus = CInt

-- | An OpenNI action that can fail with a `Status`.
type Oni a = IO (Either Status a)

foreign import ccall unsafe "OniCAPI.h oniInitialize"
  oniInitialize :: CInt -> IO OniStatus

-- | Numerical version of the OpenNI API.
type ApiVersion = Int


-- | Initialize OpenNI2. Must be called before any other Honi IO functions.
initialize :: ApiVersion -> IO Status
initialize version
  = fromCInt <$> oniInitialize (fromIntegral version)

-- | Shutdown OpenNI2.
foreign import ccall unsafe "OniCAPI.h oniShutdown"
  shutdown :: IO ()

foreign import ccall unsafe "OniCAPI.h oniGetDeviceList"
  oniGetDeviceList :: Ptr (Ptr DeviceInfo) -> Ptr CInt -> IO OniStatus

foreign import ccall unsafe "OniCAPI.h oniReleaseDeviceList"
  oniReleaseDeviceList :: Ptr DeviceInfo -> IO OniStatus

-- | Get the list of currently connected device.
-- Each device is represented by its `DeviceInfo`.
getDeviceList :: Oni [ DeviceInfo ]
getDeviceList = alloca $ \listPtr -> alloca $ \numPtr ->
  whenOK (oniGetDeviceList listPtr numPtr) $ do
    num <- peek numPtr
    list <- peek listPtr
    deviceList <- peekArray (fromIntegral num) list
    oniReleaseDeviceList list
    return $ Right deviceList

-- | Run the given action if `StatusOK` was returned from the first one.
whenOK :: IO OniStatus -> Oni a -> Oni a
whenOK oni ok = do
  r <- oni
  case fromCInt r of
    StatusOK -> ok
    err -> return $ Left err

foreign import ccall unsafe "OniCAPI.h oniDeviceOpen"
  oniDeviceOpen :: CString -> Ptr OpaquePtr -> IO OniStatus

-- | Open a device from a device URI.
deviceOpen :: BS.ByteString -> Oni DeviceHandle
deviceOpen devUri
  = BS.useAsCString devUri $ \uriPtr ->
      alloca $ \handlePtr ->
        whenOK (oniDeviceOpen uriPtr handlePtr)
          ((Right . DeviceHandle) <$> peek handlePtr)

-- | Open a device from a `DeviceInfo`.
deviceOpenInfo :: DeviceInfo -> Oni DeviceHandle
deviceOpenInfo = deviceOpen . uri

foreign import ccall unsafe "OniCAPI.h oniDeviceClose"
  oniDeviceClose :: OpaquePtr -> IO OniStatus

-- | Close a device.
deviceClose :: DeviceHandle -> IO Status
deviceClose (DeviceHandle p) = fromCInt <$> oniDeviceClose p

foreign import ccall unsafe "OniCAPI.h oniDeviceGetSensorInfo"
  oniDeviceGetSensorInfo :: OpaquePtr -> CInt -> IO (Ptr SensorInfo)

-- | Get the possible configurations available for a specific source,
-- or @Nothing@ if the source does not exist.
deviceGetSensorInfo :: DeviceHandle -> SensorType -> IO (Maybe SensorInfo)
deviceGetSensorInfo (DeviceHandle p) sType = do
  sip <- oniDeviceGetSensorInfo p (toCInt sType)
  if sip == nullPtr
    then return Nothing
    else do
      sensorInfo <- peek sip
      return $ Just sensorInfo

foreign import ccall unsafe "OniCAPI.h oniDeviceCreateStream"
  oniDeviceCreateStream :: OpaquePtr -> CInt -> Ptr OpaquePtr -> IO OniStatus

-- | Create a new stream in the device. The stream will originate from the source.
deviceCreateStream :: DeviceHandle -> SensorType -> Oni StreamHandle
deviceCreateStream (DeviceHandle dh) st = alloca $ \streamPtr ->
  whenOK (oniDeviceCreateStream dh (toCInt st) streamPtr)
    ((Right . StreamHandle) <$> peek streamPtr)

foreign import ccall unsafe "OniCAPI.h oniStreamStart"
  oniStreamStart :: OpaquePtr -> IO OniStatus

-- | Start generating data from the stream.
streamStart :: StreamHandle -> IO Status
streamStart (StreamHandle sh) = fromCInt <$> oniStreamStart sh

foreign import ccall unsafe "OniCAPI.h oniStreamStop"
  oniStreamStop :: OpaquePtr -> IO ()

-- | Stop generating data from the stream.
streamStop :: StreamHandle -> IO ()
streamStop (StreamHandle sh) = oniStreamStop sh
