module DevelMain where

import Main (defaultMain)
import Control.Concurrent
import Control.Monad (void)
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.Store (Store(..), lookupStore, readStore, storeAction, withStore)
import GHC.Word (Word32)

update :: IO ()
update = do
  lookupStore tidStoreNum >>= maybe setupNew restart
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0
    setupNew :: IO ()
    setupNew = do
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      void $ storeAction (Store tidStoreNum) (newIORef tid)
    restart tidStore = modifyStoredIORef tidStore $ \tid -> do
      killThread tid
      withStore doneStore takeMVar
      readStore doneStore >>= start
    start :: MVar () -> IO ThreadId
    start done = forkFinally defaultMain (\_ -> putMVar done ())

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref

tidStoreNum :: Word32
tidStoreNum = 1
