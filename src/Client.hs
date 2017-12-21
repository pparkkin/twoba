module Client where

import Control.Concurrent ( forkIO )
import Data.Text ( unpack
                 , pack )
import Network.WebSockets ( Connection
                          , runClient
                          , receiveData
                          , sendTextData )

import Types

testClient :: IO ()
testClient = do
  runClient "localhost" 3000 "" start
  where
    start :: Connection -> IO ()
    start conn = do
      forkIO (connListener conn)
      stdinListener conn
    connListener :: Connection -> IO ()
    connListener conn = do
      d <- receiveData conn
      putStrLn (unpack d)
      connListener conn
    stdinListener :: Connection -> IO ()
    stdinListener conn = do
      l <- getLine
      sendTextData conn (pack l)
      stdinListener conn
