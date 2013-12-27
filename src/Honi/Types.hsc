{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls, DeriveDataTypeable #-}

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
  , timeoutNone
  , timeoutForever
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
    (cIntToInt <$> #{peek OniVersion, major}       ptr) <*>
    (cIntToInt <$> #{peek OniVersion, minor}       ptr) <*>
    (cIntToInt <$> #{peek OniVersion, maintenance} ptr) <*>
    (cIntToInt <$> #{peek OniVersion, build}       ptr)

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
  toCInt SensorDepth = #const ONI_SENSOR_COLOR
  toCInt SensorColor = #const ONI_SENSOR_DEPTH
  fromCInt (#const ONI_SENSOR_IR)    = SensorIR
  fromCInt (#const ONI_SENSOR_COLOR) = SensorDepth
  fromCInt (#const ONI_SENSOR_DEPTH) = SensorColor
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
  } deriving Show

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral

instance Storable VideoMode where
  alignment _ = #{alignment OniVideoMode}
  sizeOf _ = #{size OniVideoMode}
  peek ptr = VideoMode <$>
    (fromCInt <$> #{peek OniVideoMode, pixelFormat} ptr) <*>
    (cIntToInt <$> #{peek OniVideoMode, resolutionX} ptr) <*>
    (cIntToInt <$> #{peek OniVideoMode, resolutionY} ptr) <*>
    (cIntToInt <$> #{peek OniVideoMode, fps} ptr)

-- | List of supported video modes by a specific source.
data SensorInfo = SensorInfo
  { sensorType :: SensorType
  , supportedVideoModes :: [ VideoMode ]
  } deriving Show

instance Storable SensorInfo where
  alignment _ = #{alignment OniSensorInfo}
  sizeOf _ = #{size OniSensorInfo}
  peek ptr = do
    st <- fromCInt <$> #{peek OniSensorInfo, sensorType} ptr
    nvm <- #{peek OniSensorInfo, numSupportedVideoModes} ptr
    vmsPtr <- #{peek OniSensorInfo, pSupportedVideoModes} ptr
    vms <- peekArray (cIntToInt nvm) vmsPtr
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


data Opaque

-- | A void pointer, not specifying what it points to.
type OpaquePtr = Ptr Opaque

-- | An opened OpenNI device.
newtype DeviceHandle = DeviceHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

-- | A handle to an OpenNI stream.
newtype StreamHandle = StreamHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

-- | A handle to an OpenNI recorder.
newtype RecorderHandle = RecorderHandle OpaquePtr
  deriving ( Eq, Ord, Storable )
