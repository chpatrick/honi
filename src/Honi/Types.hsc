{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls, DeriveDataTypeable #-}

#include "OniCAPI.h"

module Honi.Types
  ( Status(..)
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

class CEnum a where
  fromCInt :: CInt -> a
  toCInt :: a -> CInt

data Status
  = StatusOK
  | StatusError
  | StatusNotImplemented
  | StatusNotSupported
  | StatusBadParameter
  | StatusOutOfFlow
  | StatusNoDevice
  | StatusTimeOut
    deriving ( Bounded, Enum, Eq, Ord, Show )

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

data SensorType
  = SensorIR
  | SensorColor
  | SensorDepth
    deriving ( Bounded, Enum, Eq, Ord, Show )

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

type OpaquePtr = Ptr Opaque

newtype DeviceHandle = DeviceHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

newtype StreamHandle = StreamHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

newtype RecorderHandle = RecorderHandle OpaquePtr
  deriving ( Eq, Ord, Storable )
