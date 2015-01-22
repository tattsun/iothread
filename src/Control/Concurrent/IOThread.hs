module Control.Concurrent.IOThread
       ( IOThread
       , new
       , add
       ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan ()
import           Control.Monad           (void)

newtype IOThread = IOThread { ioThread :: Chan (IO ()) }

-- | Get IOThread and start running worker thread.
new :: IO IOThread
new = do
  iot <- IOThread <$> newChan
  void $ forkIO (worker iot)
  return iot

-- | Deligate your action to worker thread.
add :: IOThread -> IO () -> IO ()
add iot act = writeChan (ioThread iot) act

worker :: IOThread -> IO ()
worker iot = do
  act <- readChan (ioThread iot)
  act
  worker iot
