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
  , PixelFormat(..)
  , VideoMode(..)
  , SensorType(..), SensorInfo(..)
  , DeviceInfo(..), DeviceHandle(..)
  , StreamHandle(..)
  , RecorderHandle(..)
  , OpaquePtr
  , CEnum(..)
  , HoniBug(..)
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
  toCInt StatusOK = 0
  toCInt StatusError = 1
  toCInt StatusNotImplemented = 2
  toCInt StatusNotSupported = 3
  toCInt StatusBadParameter = 4
  toCInt StatusOutOfFlow = 5
  toCInt StatusNoDevice = 6
  toCInt StatusTimeOut = 102
  fromCInt 0 = StatusOK
  fromCInt 1 = StatusError
  fromCInt 2 = StatusNotImplemented
  fromCInt 3 = StatusNotSupported
  fromCInt 4 = StatusBadParameter
  fromCInt 5 = StatusOutOfFlow
  fromCInt 6 = StatusNoDevice
  fromCInt 102 = StatusTimeOut
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
  toCInt Depth1MM = 100
  toCInt Depth100UM = 101
  toCInt Shift9_2 = 102
  toCInt Shift9_3 = 103
  toCInt RGB888 = 200
  toCInt YUV422 = 201
  toCInt Gray8 = 202
  toCInt Gray16 = 203
  toCInt JPEG = 204
  toCInt YUVY = 205
  fromCInt 100 = Depth1MM
  fromCInt 101 = Depth100UM
  fromCInt 102 = Shift9_2
  fromCInt 103 = Shift9_3
  fromCInt 200 = RGB888
  fromCInt 201 = YUV422
  fromCInt 202 = Gray8
  fromCInt 203 = Gray16
  fromCInt 204 = JPEG
  fromCInt 205 = YUVY
  fromCInt i = throw (HoniBugUnknownCEnum "PixelFormat" i)

instance CEnum SensorType where
  toCInt SensorIR = 1
  toCInt SensorDepth = 2
  toCInt SensorColor = 3
  fromCInt 1 = SensorIR
  fromCInt 2 = SensorDepth
  fromCInt 3 = SensorColor
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
