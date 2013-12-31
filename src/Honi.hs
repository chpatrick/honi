{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

-- | Binding for the OpenNI C API (OniCAPI.h).
--
-- Example use:
--
-- >import Honi
-- >import Honi.Types
-- >
-- >initialize oniApiVersion
-- >Right (di:_) <- getDeviceList
-- >Right d      <- deviceOpenInfo di
-- >deviceGetSensorInfo d SensorDepth
--
-- This module does not use `error`.
-- All its potential errors can be caught as `HoniBug`.
module Honi
  ( ApiVersion
  , Oni
  , oniApiVersion

  -- * General
  , initialize, shutdown
  , getDeviceList
  , deviceOpen
  , deviceOpenInfo
  , deviceClose
  , deviceGetSensorInfo
  , deviceGetInfo
  , deviceCreateStream
  , enableDepthColorSync
  , disableDepthColorSync
  , getDepthColorSyncEnabled
  , waitForAnyStream
  , bytesPerPixel
  , getVersion
  , getExtendedError

  -- * Streams
  , streamStart
  , streamStop
  , streamReadFrame

  -- * Timeouts
  , OniTimeout
  , timeoutNone
  , timeoutForever

  -- * Internal
  , bytesPerPixelIO
  )
where

import Control.Applicative
import Control.Exception (throwIO)
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


-- | Initialize OpenNI2. Must be called before any other Honi IO functions.
-- Use `oniApiVersion` for the OpenNI version this binding was compiled against.
initialize :: ApiVersion -> IO Status
initialize version
  = fromCInt <$> oniInitialize (cint version)

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
    deviceList <- peekArray (int num) list
    status <- oniReleaseDeviceList list
    case fromCInt status of
      StatusOK -> return $ Right deviceList
      err      -> throwIO err

-- | Run the given action if `StatusOK` was returned from the first one.
whenOK :: IO OniStatus -> Oni a -> Oni a
whenOK oni ok = do
  r <- oni
  case fromCInt r of
    StatusOK -> ok
    err -> return $ Left err

-- Monomorphic versions of fromIntegral to make code clear.
int :: CInt -> Int
int = fromIntegral

cint :: Int -> CInt
cint = fromIntegral


foreign import ccall unsafe "OniCAPI.h oniDeviceOpen"
  oniDeviceOpen :: CString -> Ptr OpaquePtr -> IO OniStatus

-- | Open a device from a device URI.
deviceOpen :: BS.ByteString -> Oni DeviceHandle
deviceOpen devUri
  = BS.useAsCString devUri $ \uriPtr ->
      alloca $ \handlePtr ->
        whenOK (oniDeviceOpen uriPtr handlePtr) $ do
          Right . DeviceHandle <$> peek handlePtr

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

foreign import ccall unsafe "OniCAPI.h oniDeviceGetInfo"
  oniDeviceGetInfo :: OpaquePtr -> Ptr DeviceInfo -> IO OniStatus

-- | Get the `DeviceInfo` of a certain device.
deviceGetInfo :: DeviceHandle -> Oni DeviceInfo
deviceGetInfo (DeviceHandle dh) = do
  alloca $ \diPtr ->
    whenOK (oniDeviceGetInfo dh diPtr) $ do
      Right <$> peek diPtr

foreign import ccall unsafe "OniCAPI.h oniDeviceCreateStream"
  oniDeviceCreateStream :: OpaquePtr -> CInt -> Ptr OpaquePtr -> IO OniStatus

-- | Create a new stream in the device. The stream will originate from the source.
deviceCreateStream :: DeviceHandle -> SensorType -> Oni StreamHandle
deviceCreateStream (DeviceHandle dh) st = alloca $ \streamPtr ->
  whenOK (oniDeviceCreateStream dh (toCInt st) streamPtr) $ do
    Right . StreamHandle <$> peek streamPtr

foreign import ccall unsafe "OniCAPI.h oniDeviceEnableDepthColorSync"
  oniDeviceEnableDepthColorSync :: OpaquePtr -> IO OniStatus

enableDepthColorSync :: DeviceHandle -> Oni ()
enableDepthColorSync (DeviceHandle dh) = do
  whenOK (oniDeviceEnableDepthColorSync dh) (return $ Right ())

foreign import ccall unsafe "OniCAPI.h oniDeviceDisableDepthColorSync"
  oniDeviceDisableDepthColorSync :: OpaquePtr -> IO ()

disableDepthColorSync :: DeviceHandle -> IO ()
disableDepthColorSync (DeviceHandle dh) = do
  oniDeviceDisableDepthColorSync dh

foreign import ccall unsafe "OniCAPI.h oniDeviceGetDepthColorSyncEnabled"
  oniDeviceGetDepthColorSyncEnabled :: OpaquePtr -> IO OniStatus

getDepthColorSyncEnabled :: DeviceHandle -> Oni ()
getDepthColorSyncEnabled (DeviceHandle dh) = do
  whenOK (oniDeviceGetDepthColorSyncEnabled dh) (return $ Right ())


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

foreign import ccall unsafe "OniCAPI.h oniWaitForAnyStream"
  oniWaitForAnyStream :: Ptr StreamHandle -> CInt -> Ptr CInt -> CInt -> IO OniStatus
  --                     [N many streams] -> N -> index of filled steam -> timeout

-- | Wait for any of the streams to have a new frame.
-- Returns the stream that has a new frame.
--
-- If you need to know the index of the stream with a new frame, you can use
-- the Eq instance of `StreamHandle`.
waitForAnyStream :: [ StreamHandle ]
                 -> OniTimeout       -- ^ Timeout (milliseconds)
                 -> Oni StreamHandle
waitForAnyStream streams (OniTimeout timeout) = withArrayLen streams $ \n streamsPtr -> do
  alloca $ \(streamIndexPtr :: Ptr CInt) -> do
    whenOK (oniWaitForAnyStream streamsPtr (cint n) streamIndexPtr (cint timeout)) $ do
      i      <- peek streamIndexPtr
      stream <- peek (advancePtr streamsPtr (int i))
      return $ Right stream

-- Helper for oniGetVersion (FFI cannot interface with structs returned by value)
foreign import ccall unsafe "helpers.h helper_oniGetVersion"
  helper_oniGetVersion :: IO (Ptr OniVersion)

-- | Get the current version of OpenNI2.
getVersion :: IO OniVersion
getVersion = peek =<< helper_oniGetVersion

foreign import ccall unsafe "OniCAPI.h oniGetExtendedError"
  oniGetExtendedError :: IO CString

-- | Returns an additional, human-readable information about the LAST OpenNI error.
-- This is the last global error, so don't rely on it when using multiple
-- threads using OpenNI. It also doesn't look like it's thread-safe AT ALL,
-- so better don't use it at all when using multiple threads using OpenNI.
getExtendedError :: IO String
getExtendedError = do
  charPtr <- oniGetExtendedError
  peekCAString charPtr

foreign import ccall unsafe "OniCAPI.h oniFormatBytesPerPixel"
  oniFormatBytesPerPixel :: CInt -> IO CInt

-- | Exposed for testing only.
bytesPerPixelIO :: PixelFormat -> IO Int
bytesPerPixelIO pf = int <$> oniFormatBytesPerPixel (toCInt pf)

-- | Translate from format to number of bytes per pixel.
-- Will return 0 for formats in which the number of bytes per pixel isn't fixed.
--
-- (This is implemented ourselves to have it pure;
-- the C version resets with the global error message logger.
-- Equivalence with the C version is covered by test suite.)
bytesPerPixel :: PixelFormat -> Int
bytesPerPixel pf = case pf of
  Gray8      -> 1

  Depth1MM   -> 2
  Depth100UM -> 2
  Shift9_2   -> 2
  Shift9_3   -> 2
  Gray16     -> 2

  RGB888     -> 3

  YUV422     -> 2
  YUVY       -> 2

  JPEG       -> 1

foreign import ccall unsafe "OniCAPI.h oniStreamReadFrame"
  oniStreamReadFrame :: OpaquePtr -> Ptr (Ptr OniFrame) -> IO OniStatus

-- | Get the next frame from the stream.
-- This function is blocking until there is a new frame from the stream.
-- For timeout, use `waitForStreams` first.
streamReadFrame :: StreamHandle -> Oni OniFrame
streamReadFrame (StreamHandle sh) = do
  alloca $ \oniFramePtrPtr -> do
    whenOK (oniStreamReadFrame sh oniFramePtrPtr) $ do
      oniFramePtr <- peek oniFramePtrPtr
      oniFrame <- peek oniFramePtr
      return $ Right oniFrame
