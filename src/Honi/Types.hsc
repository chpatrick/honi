{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls, DeriveDataTypeable, NamedFieldPuns #-}

#include "OniCAPI.h"
#include "OniVersion.h"

-- | Low-level OpenNI types.
--
-- This module does not use `error`.
-- All its potential errors can be caught as `HoniBug`.
module Honi.Types
  ( Status(..)
  , ApiVersion
  , oniApiVersion
  , OniVersion(..)
  , PixelFormat(..)
  , VideoMode(..)
  , SensorType(..), SensorInfo(..)
  , DeviceInfo(..), DeviceHandle(..)
  , StreamHandle(..)
  , RecorderHandle(..)
  , OpaquePtr
  , CEnum(..)
  , HoniBug(..)
  , OniTimeout(..)
  , DeviceCallbacks(..), CallbackHandle(..)
  , timeoutNone
  , timeoutForever
  , OniFrame(..)

  -- * Internal
  , C_DeviceCallbacks(..)
  , C_OniDeviceInfoCallback
  , C_OniDeviceStateCallback
  ) where

import Control.Applicative
import Control.Exception
import Data.Word
import qualified Data.ByteString as BS
import Data.Typeable
import Foreign
import Foreign.C

-- | Numerical version of the OpenNI API.
type ApiVersion = Int

-- | The @ONI_API_VERSION@ against which OpenNI was compiled.
oniApiVersion :: ApiVersion
oniApiVersion = #const ONI_API_VERSION

-- | Holds an OpenNI version number, which consists of four separate numbers.
data OniVersion = OniVersion
  { oniVersionMajor       :: Int
  , oniVersionMinor       :: Int
  , oniVersionMaintenance :: Int
  , oniVersionBuild       :: Int
  } deriving ( Eq, Ord, Show )

instance Storable OniVersion where
  alignment _ = #{alignment OniVersion}
  sizeOf _ = #{size OniVersion}
  peek ptr = OniVersion <$>
    (int <$> #{peek OniVersion, major}       ptr) <*>
    (int <$> #{peek OniVersion, minor}       ptr) <*>
    (int <$> #{peek OniVersion, maintenance} ptr) <*>
    (int <$> #{peek OniVersion, build}       ptr)

-- | Converts between a plain Haskell sum type enumeration and a `CInt`.
class CEnum a where
  fromCInt :: CInt -> a
  toCInt :: a -> CInt

-- | Possible OpenNI failure values.
data Status
  = StatusOK
  | StatusError
  | StatusNotImplemented
  | StatusNotSupported
  | StatusBadParameter
  | StatusOutOfFlow
  | StatusNoDevice
  | StatusTimeOut
    deriving ( Bounded, Enum, Eq, Ord, Show, Typeable )

-- | Exception instance for Status so that we can throw it
-- in cases where @StatusOK@ is expected from the implementation
-- but we would have to ignore the return value otherwise.
instance Exception Status

-- | Exception to be thrown for bugs in this OpenNI binding.
data HoniBug
  = HoniBugUnknownCEnum String CInt
    deriving ( Eq, Ord, Show, Typeable )

instance Exception HoniBug

instance CEnum Status where
  toCInt StatusOK             = #const ONI_STATUS_OK
  toCInt StatusError          = #const ONI_STATUS_ERROR
  toCInt StatusNotImplemented = #const ONI_STATUS_NOT_IMPLEMENTED
  toCInt StatusNotSupported   = #const ONI_STATUS_NOT_SUPPORTED
  toCInt StatusBadParameter   = #const ONI_STATUS_BAD_PARAMETER
  toCInt StatusOutOfFlow      = #const ONI_STATUS_OUT_OF_FLOW
  toCInt StatusNoDevice       = #const ONI_STATUS_NO_DEVICE
  toCInt StatusTimeOut        = #const ONI_STATUS_TIME_OUT
  fromCInt (#const ONI_STATUS_OK)              = StatusOK
  fromCInt (#const ONI_STATUS_ERROR)           = StatusError
  fromCInt (#const ONI_STATUS_NOT_IMPLEMENTED) = StatusNotImplemented
  fromCInt (#const ONI_STATUS_NOT_SUPPORTED)   = StatusNotSupported
  fromCInt (#const ONI_STATUS_BAD_PARAMETER)   = StatusBadParameter
  fromCInt (#const ONI_STATUS_OUT_OF_FLOW)     = StatusOutOfFlow
  fromCInt (#const ONI_STATUS_NO_DEVICE)       = StatusNoDevice
  fromCInt (#const ONI_STATUS_TIME_OUT)        = StatusTimeOut
  fromCInt i = throw (HoniBugUnknownCEnum "Status" i)

-- | The source of the stream.
data SensorType
  = SensorIR
  | SensorColor
  | SensorDepth
    deriving ( Bounded, Enum, Eq, Ord, Show )

-- | All available formats of the output of a stream.
data PixelFormat
  = Depth1MM
  | Depth100UM
  | Shift9_2
  | Shift9_3
  | RGB888
  | YUV422
  | Gray8
  | Gray16
  | JPEG
  | YUVY
    deriving ( Bounded, Enum, Show, Ord, Eq )

instance CEnum PixelFormat where
  toCInt Depth1MM   = #const ONI_PIXEL_FORMAT_DEPTH_1_MM
  toCInt Depth100UM = #const ONI_PIXEL_FORMAT_DEPTH_100_UM
  toCInt Shift9_2   = #const ONI_PIXEL_FORMAT_SHIFT_9_2
  toCInt Shift9_3   = #const ONI_PIXEL_FORMAT_SHIFT_9_3
  toCInt RGB888     = #const ONI_PIXEL_FORMAT_RGB888
  toCInt YUV422     = #const ONI_PIXEL_FORMAT_YUV422
  toCInt Gray8      = #const ONI_PIXEL_FORMAT_GRAY8
  toCInt Gray16     = #const ONI_PIXEL_FORMAT_GRAY16
  toCInt JPEG       = #const ONI_PIXEL_FORMAT_JPEG
  toCInt YUVY       = #const ONI_PIXEL_FORMAT_YUYV
  fromCInt (#const ONI_PIXEL_FORMAT_DEPTH_1_MM)   = Depth1MM
  fromCInt (#const ONI_PIXEL_FORMAT_DEPTH_100_UM) = Depth100UM
  fromCInt (#const ONI_PIXEL_FORMAT_SHIFT_9_2)    = Shift9_2
  fromCInt (#const ONI_PIXEL_FORMAT_SHIFT_9_3)    = Shift9_3
  fromCInt (#const ONI_PIXEL_FORMAT_RGB888)       = RGB888
  fromCInt (#const ONI_PIXEL_FORMAT_YUV422)       = YUV422
  fromCInt (#const ONI_PIXEL_FORMAT_GRAY8)        = Gray8
  fromCInt (#const ONI_PIXEL_FORMAT_GRAY16)       = Gray16
  fromCInt (#const ONI_PIXEL_FORMAT_JPEG)         = JPEG
  fromCInt (#const ONI_PIXEL_FORMAT_YUYV)         = YUVY
  fromCInt i = throw (HoniBugUnknownCEnum "PixelFormat" i)

instance CEnum SensorType where
  toCInt SensorIR    = #const ONI_SENSOR_IR
  toCInt SensorColor = #const ONI_SENSOR_COLOR
  toCInt SensorDepth = #const ONI_SENSOR_DEPTH
  fromCInt (#const ONI_SENSOR_IR)    = SensorIR
  fromCInt (#const ONI_SENSOR_COLOR) = SensorColor
  fromCInt (#const ONI_SENSOR_DEPTH) = SensorDepth
  fromCInt i = throw (HoniBugUnknownCEnum "SensorType" i)

-- | Basic description of a device.
data DeviceInfo = DeviceInfo
  { uri :: BS.ByteString
  , vendor :: BS.ByteString
  , name :: BS.ByteString
  , usbVendorID :: Word16
  , usbProductID :: Word16
  } deriving Show

instance Storable DeviceInfo where
  alignment _ = #{alignment OniDeviceInfo}
  sizeOf _ = #{size OniDeviceInfo}
  peek ptr = DeviceInfo <$>
    BS.packCString (#{ptr OniDeviceInfo, uri} ptr) <*>
    BS.packCString (#{ptr OniDeviceInfo, vendor} ptr) <*>
    BS.packCString (#{ptr OniDeviceInfo, name} ptr) <*>
    #{peek OniDeviceInfo, usbVendorId} ptr <*>
    #{peek OniDeviceInfo, usbProductId} ptr

-- | Description of the output: format and resolution.
data VideoMode = VideoMode
  { pixelFormat :: PixelFormat
  , resolutionX :: Int
  , resolutionY :: Int
  , fps :: Int
  } deriving ( Eq, Ord, Show )


instance Storable VideoMode where
  alignment _ = #{alignment OniVideoMode}
  sizeOf _ = #{size OniVideoMode}
  peek ptr = VideoMode <$>
    (fromCInt <$> #{peek OniVideoMode, pixelFormat} ptr) <*>
    (int <$> #{peek OniVideoMode, resolutionX} ptr) <*>
    (int <$> #{peek OniVideoMode, resolutionY} ptr) <*>
    (int <$> #{peek OniVideoMode, fps} ptr)

-- | List of supported video modes by a specific source.
data SensorInfo = SensorInfo
  { sensorType :: SensorType
  , supportedVideoModes :: [ VideoMode ]
  } deriving ( Eq, Ord, Show )

instance Storable SensorInfo where
  alignment _ = #{alignment OniSensorInfo}
  sizeOf _ = #{size OniSensorInfo}
  peek ptr = do
    st <- fromCInt <$> #{peek OniSensorInfo, sensorType} ptr
    nvm <- #{peek OniSensorInfo, numSupportedVideoModes} ptr
    vmsPtr <- #{peek OniSensorInfo, pSupportedVideoModes} ptr
    vms <- peekArray (int nvm) vmsPtr
    return $ SensorInfo st vms

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- | Timeout in milliseconds.
-- Must be positive or `timeoutForever` or `timeoutNone`.
newtype OniTimeout = OniTimeout Int
  deriving ( Show, Ord, Eq )

-- | No timeout (for immediate return).
timeoutNone :: OniTimeout
timeoutNone = OniTimeout (#const ONI_TIMEOUT_NONE)

-- | Infinite timeout.
timeoutForever :: OniTimeout
timeoutForever = OniTimeout (#const ONI_TIMEOUT_FOREVER)

-- | State a device can be in.
data DeviceState
  = StateOK
  | StateError
  | StateNotReady
  | StateEOF
    deriving ( Bounded, Enum, Eq, Ord, Show )

instance CEnum DeviceState where
  toCInt StateOK       = #const ONI_DEVICE_STATE_OK
  toCInt StateError    = #const ONI_DEVICE_STATE_ERROR
  toCInt StateNotReady = #const ONI_DEVICE_STATE_NOT_READY
  toCInt StateEOF      = #const ONI_DEVICE_STATE_EOF
  fromCInt (#const ONI_DEVICE_STATE_OK)        = StateOK
  fromCInt (#const ONI_DEVICE_STATE_ERROR)     = StateError
  fromCInt (#const ONI_DEVICE_STATE_NOT_READY) = StateNotReady
  fromCInt (#const ONI_DEVICE_STATE_EOF)       = StateEOF
  fromCInt i = throw (HoniBugUnknownCEnum "DeviceState" i)

data DeviceCallbacks = DeviceCallbacks
  { deviceConnected    :: DeviceInfo -> IO ()
  , deviceDisconnected :: DeviceInfo -> IO ()
  , deviceStateChanged :: DeviceInfo -> DeviceState -> IO ()
  }

instance Show DeviceCallbacks where
  show DeviceCallbacks{} = "DeviceCallbacks"

type C_OniDeviceInfoCallback = Ptr DeviceInfo -> OpaquePtr -> IO ()
--                             const OniDeviceInfo* pInfo -> void* pCookie
type C_OniDeviceStateCallback = Ptr DeviceInfo -> CInt -> OpaquePtr -> IO ()
--                              const OniDeviceInfo* pInfo -> OniDeviceState deviceState -> void* pCookie

data C_DeviceCallbacks = C_DeviceCallbacks
  { _deviceConnected    :: FunPtr C_OniDeviceInfoCallback
  , _deviceDisconnected :: FunPtr C_OniDeviceInfoCallback
  , _deviceStateChanged :: FunPtr C_OniDeviceStateCallback
  } deriving ( Eq, Ord )

instance Storable C_DeviceCallbacks where
  alignment _ = #{alignment OniDeviceCallbacks}
  sizeOf _ = #{size OniDeviceCallbacks}
  -- TODO error peek?
  poke ptr (C_DeviceCallbacks connPtr disPtr changedPtr) = do
    #{poke OniDeviceCallbacks, deviceConnected}    ptr connPtr
    #{poke OniDeviceCallbacks, deviceDisconnected} ptr disPtr
    #{poke OniDeviceCallbacks, deviceStateChanged} ptr changedPtr


data Opaque

-- | A void pointer, not specifying what it points to.
type OpaquePtr = Ptr Opaque

-- | An opened OpenNI device.
newtype DeviceHandle = DeviceHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

-- | A registered OpenNI callback.
data CallbackHandle = CallbackHandle
  OpaquePtr               -- OpenNI handle pointer
  C_DeviceCallbacks       -- callback FunPtrs. Each must be freed on unregister
  (Ptr C_DeviceCallbacks) -- Space where callback FunPtrs are stored. Also must be freed on unregister
  deriving ( Eq, Ord )

-- | A handle to an OpenNI stream.
newtype StreamHandle = StreamHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

-- | A handle to an OpenNI recorder.
newtype RecorderHandle = RecorderHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

-- Monomorphic versions of fromIntegral to make code clear.
int :: CInt -> Int
int = fromIntegral

-- | `OniFrame` with C types.
data C_OniFrame = C_OniFrame
  { _frameData :: BS.ByteString

  , _frameSensorType :: SensorType
  , _frameTimestamp :: Word64
  , _frameIndex :: CInt

  , _frameWidth :: CInt
  , _frameHeight :: CInt

  , _frameVideoMode :: VideoMode
  , _frameCroppingEnabled :: Bool
  , _frameCropOriginX :: CInt
  , _frameCropOriginY :: CInt

  , _frameStride :: CInt
  } deriving ( Eq, Ord, Show )

-- | All information of the current frame.
data OniFrame = OniFrame
  { frameData :: BS.ByteString

  , frameSensorType :: SensorType
  , frameTimestamp :: Word64
  , frameIndex :: Int

  , frameWidth :: Int
  , frameHeight :: Int

  , frameVideoMode :: VideoMode
  , frameCroppingEnabled :: Bool
  , frameCropOriginX :: Int
  , frameCropOriginY :: Int

  , frameStride :: Int
  } deriving ( Eq, Ord, Show )

instance Storable C_OniFrame where
  alignment _ = #{alignment OniFrame}
  sizeOf _ = #{size OniFrame}
  peek ptr = do
    len     <- int <$> #{peek OniFrame, dataSize} ptr
    dataPtr <- #{peek OniFrame, data} ptr

    -- TODO account for stride
    C_OniFrame
      <$> BS.packCStringLen (dataPtr, len)
      <*> (fromCInt <$> #{peek OniFrame, sensorType} ptr)
      <*> (#{peek OniFrame, timestamp} ptr)
      <*> (#{peek OniFrame, frameIndex} ptr)
      <*> (#{peek OniFrame, width} ptr)
      <*> (#{peek OniFrame, height} ptr)
      <*> (#{peek OniFrame, videoMode} ptr)
      <*> (#{peek OniFrame, croppingEnabled} ptr)
      <*> (#{peek OniFrame, cropOriginX} ptr)
      <*> (#{peek OniFrame, cropOriginY} ptr)
      <*> (#{peek OniFrame, stride} ptr)

instance Storable OniFrame where
  alignment _ = #{alignment OniFrame}
  sizeOf _ = #{size OniFrame}
  peek ptr = do
    C_OniFrame
      { _frameData
      , _frameSensorType
      , _frameTimestamp
      , _frameIndex
      , _frameWidth
      , _frameHeight
      , _frameVideoMode
      , _frameCroppingEnabled
      , _frameCropOriginX
      , _frameCropOriginY
      , _frameStride
      } <- peek (castPtr ptr)
    return $ OniFrame
      { frameData            = _frameData
      , frameSensorType      = _frameSensorType
      , frameTimestamp       = _frameTimestamp
      , frameIndex           = int _frameIndex
      , frameWidth           = int _frameWidth
      , frameHeight          = int _frameHeight
      , frameVideoMode       = _frameVideoMode
      , frameCroppingEnabled = _frameCroppingEnabled
      , frameCropOriginX     = int _frameCropOriginX
      , frameCropOriginY     = int _frameCropOriginY
      , frameStride          = int _frameStride
      }
