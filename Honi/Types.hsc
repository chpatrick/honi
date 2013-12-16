{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls #-}

#include "OniCAPI.h"

module Honi.Types
  ( Status(..)
  , DeviceInfo(..)
  , DeviceHandle(..)
  , StreamHandle(..)
  , RecorderHandle(..)
  , OpaquePtr
  ) where

import Control.Applicative
import Data.Word
import qualified Data.ByteString as BS

import Control.Applicative
import Data.Word
import qualified Data.ByteString as BS
import Foreign

data Status
  = StatusOK
  | StatusError
  | StatusNotImplemented
  | StatusNotSupported
  | StatusBadParameter
  | StatusOutOfFlow
  | StatusNoDevice
  | StatusTimeOut
    deriving ( Eq, Ord, Show )

instance Enum Status where
  fromEnum StatusOK = 0
  fromEnum StatusError = 1
  fromEnum StatusNotImplemented = 2
  fromEnum StatusNotSupported = 3
  fromEnum StatusBadParameter = 4
  fromEnum StatusOutOfFlow = 5
  fromEnum StatusNoDevice = 6
  fromEnum StatusTimeOut = 102
  toEnum 0 = StatusOK
  toEnum 1 = StatusError
  toEnum 2 = StatusNotImplemented
  toEnum 3 = StatusNotSupported
  toEnum 4 = StatusBadParameter
  toEnum 5 = StatusOutOfFlow
  toEnum 6 = StatusNoDevice
  toEnum 102 = StatusTimeOut

data SensorType
  = SensorIR
  | SensorColor
  | SensorDepth
    deriving ( Eq, Ord, Show )

instance Enum SensorType where
  fromEnum SensorIR = 1
  fromEnum SensorDepth = 2
  fromEnum SensorColor = 3
  toEnum 1 = SensorIR
  toEnum 2 = SensorDepth
  toEnum 3 = SensorColor

data DeviceInfo = DeviceInfo
  { uri :: BS.ByteString
  , vendor :: BS.ByteString
  , name :: BS.ByteString
  , usbVendorID :: Word16
  , usbProductID :: Word16
  } deriving Show

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable DeviceInfo where
  alignment _ = #{alignment OniDeviceInfo}
  sizeOf _ = #{size OniDeviceInfo}
  peek ptr = DeviceInfo <$>
    BS.packCString (#{ptr OniDeviceInfo, uri} ptr) <*>
    BS.packCString (#{ptr OniDeviceInfo, vendor} ptr) <*>
    BS.packCString (#{ptr OniDeviceInfo, name} ptr) <*>
    #{peek OniDeviceInfo, usbVendorId} ptr <*>
    #{peek OniDeviceInfo, usbProductId} ptr

data Opaque

type OpaquePtr = Ptr Opaque

newtype DeviceHandle = DeviceHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

newtype StreamHandle = StreamHandle OpaquePtr
  deriving ( Eq, Ord, Storable )

newtype RecorderHandle = RecorderHandle OpaquePtr
  deriving ( Eq, Ord, Storable )