module EventPipe where

import Control.Concurrent.STM.TVar ( TVar
                                   , newTVar
                                   , modifyTVar
                                   , swapTVar
                                   )
import Control.Monad.STM ( atomically )

data EventPipe a = EventPipe (TVar [a])

newEventPipe :: IO (EventPipe a)
newEventPipe = atomically $ do
  m <- newTVar []
  return $ EventPipe m

pushEvent :: EventPipe a -> a -> IO ()
pushEvent (EventPipe v) e = atomically $ modifyTVar v (\l -> e : l)

takeEvents :: EventPipe a -> IO [a]
takeEvents (EventPipe v) = atomically $ swapTVar v []
