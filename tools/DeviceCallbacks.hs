import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad

import Honi
import Honi.Types

main :: IO ()
main = do
  initialize oniApiVersion

  registerDeviceCallbacks DeviceCallbacks
    { deviceConnected    = \di    -> putStrLn $ "connected: "     ++ show di
    , deviceDisconnected = \di    -> putStrLn $ "disconnected: "  ++ show di
    , deviceStateChanged = \di ds -> putStrLn $ "state changed: " ++ show (di, ds)
    }

  handle (\UserInterrupt -> shutdown) $ do
    putStrLn "Listening for connecting/disconnecting devices..."
    forever (threadDelay 10000000)
