module Main where

import           Control.Exception
import           Test.Hspec

import Honi
import Honi.Types


main :: IO ()
main = hspec $ do
  describe "FFI" $ do

    let testCIntBijective typeName list =
          it (typeName ++ " numbers are bijective") $ do
            map (fromCInt . toCInt) list `shouldBe` list

    testCIntBijective "Status"      (allOf :: [Status])
    testCIntBijective "PixelFormat" (allOf :: [PixelFormat])
    testCIntBijective "SensorType"  (allOf :: [SensorType])

    it "should throw exceptions for unknown C enums" $ do
      evaluate (fromCInt 10 :: Status)      `shouldThrow` (\(HoniBugUnknownCEnum "Status"      10) -> True)
      evaluate (fromCInt 10 :: PixelFormat) `shouldThrow` (\(HoniBugUnknownCEnum "PixelFormat" 10) -> True)
      evaluate (fromCInt 10 :: SensorType)  `shouldThrow` (\(HoniBugUnknownCEnum "SensorType"  10) -> True)

  describe "General" $ do

    it "getVersion major version is 2" $ do
      OniVersion{ oniVersionMajor = m } <- getVersion
      m `shouldBe` 2

    it "getExtendedError is empty" $ do
      e <- getExtendedError
      e `shouldBe` ""


allOf :: (Bounded a, Enum a) => [a]
allOf = [minBound..]
