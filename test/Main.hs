module Main where

import           Test.Hspec

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


allOf :: (Bounded a, Enum a) => [a]
allOf = [minBound..]
