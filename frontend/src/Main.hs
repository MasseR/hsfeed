{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
module Main where

import Reflex.Dom hiding (mainWidget, run)
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import JSDOM.Window (getLocalStorage)
import JSDOM.Storage (setItem, getItem, Storage)
import JSDOM
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import Reflex.Dom.Location

data App = App { appModel :: Model }

defaultMain :: IO ()
defaultMain = run 3911 $ do
  app <- App <$> setup
  loc <- (,) <$> getLocationUrl <*> getLocationFragment
  mainWidget $ text (T.pack $ show loc)

newtype Model = Model Int deriving (Show, Read)

defaultModel :: Model
defaultModel = Model 0

setup :: MonadJSM m => m Model
setup = do
  st <- currentWindowUnchecked >>= getLocalStorage
  let key = "model" :: String
  getItem st key >>= maybe (saveNew st key) (return . read)
    where
      saveNew st key = defaultModel <$ setItem st key (show defaultModel)

getFromStorage :: (MonadJSM m, Read a) => String -> m (Maybe a)
getFromStorage key = do
  st <- currentWindowUnchecked >>= getLocalStorage
  fmap read <$> getItem st key

setToStorage :: (MonadJSM m, Show a) => String -> a -> m ()
setToStorage key val = do
  st <- currentWindowUnchecked >>= getLocalStorage
  setItem st key (show val)

main :: IO ()
main = defaultMain
