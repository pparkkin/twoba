module ConnectionMap where

import Control.Concurrent.STM.TVar ( TVar
                                   , newTVar
                                   , readTVar
                                   , writeTVar
                                   )
import Control.Monad ( when )
import Control.Monad.STM ( atomically )

import qualified Data.Map.Strict as M
import qualified Network.WebSockets as WS

data ConnectionMap a = ConnectionMap (TVar (M.Map a WS.Connection))

newConnectionMap :: IO (ConnectionMap a)
newConnectionMap = atomically $ do
  m <- newTVar M.empty
  return $ ConnectionMap m

addConnection :: Ord a => ConnectionMap a -> a -> WS.Connection -> IO ()
addConnection (ConnectionMap ps) n c = atomically $ do
  m <- readTVar ps
  when (M.size m < 2) $
    writeTVar ps (M.insert n c m)

mapConnections_ :: (a -> WS.Connection -> IO ()) -> ConnectionMap a -> IO ()
mapConnections_ a (ConnectionMap ps) = do
  m <- atomically $ readTVar ps
  sequence_ $ M.elems $ M.mapWithKey a m

connectionCount :: ConnectionMap a -> IO Int
connectionCount (ConnectionMap ps) = atomically $ do
  m <- readTVar ps
  return (M.size m)
