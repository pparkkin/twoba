module ConnectionMap where

import Control.Concurrent.STM.TVar ( TVar
                                   , newTVar
                                   , readTVar
                                   , writeTVar
                                   )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import Control.Monad.STM ( atomically )

import qualified Data.Map.Strict as M
import qualified Network.WebSockets as WS

data ConnectionMap a = ConnectionMap (TVar (M.Map a WS.Connection))

newConnectionMap :: MonadIO m => m (ConnectionMap a)
newConnectionMap = liftIO $ atomically $ do
  m <- newTVar M.empty
  return $ ConnectionMap m

addConnection :: (Ord a, MonadIO m) => ConnectionMap a -> a -> WS.Connection -> m ()
addConnection (ConnectionMap ps) n c = liftIO $ atomically $ do
  m <- readTVar ps
  when (M.size m < 2) $
    writeTVar ps (M.insert n c m)

mapConnections_ :: MonadIO m => (a -> WS.Connection -> m ()) -> ConnectionMap a -> m ()
mapConnections_ a (ConnectionMap ps) = do
  m <- liftIO $ atomically $ readTVar ps
  sequence_ $ M.elems $ M.mapWithKey a m

connectionCount :: MonadIO m => ConnectionMap a -> m Int
connectionCount (ConnectionMap ps) = liftIO $ atomically $ do
  m <- readTVar ps
  return (M.size m)
