{-# LANGUAGE ForeignFunctionInterface #-}

module Honi
  (ApiVersion
  , initialize, shutdown, withOpenNI
  )
where

import Control.Applicative
import Foreign.C

import Honi.Types

foreign import ccall unsafe "OniCAPI.h oniInitialize"
  oniInitialize :: CInt -> IO CInt

type ApiVersion = Int

initialize :: ApiVersion -> IO OniStatus
initialize version
  = (toEnum . fromIntegral) <$> oniInitialize (fromIntegral version)

foreign import ccall unsafe "OniCAPI.h oniShutdown"
  shutdown :: IO ()

withOpenNI :: ApiVersion -> IO () -> IO ()
withOpenNI version a = initialize version >> a >> shutdown