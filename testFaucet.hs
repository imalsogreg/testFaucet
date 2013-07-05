module Main where

import Control.Concurrent
import Control.Applicative
import Data.Time
import Control.Monad.State

data FaucetCommand = FaucetStart | FaucetStop | FaucetDestroy (MVar ())

data FaucetState s = FaucetState { faucetFun          :: StateT s IO ()
                                 , faucetRunning      :: Bool
                                 , faucetInitTime     :: UTCTime
                                 , faucetLastRunStart :: Maybe UTCTime
                                 , faucetLastRunEnd   :: Maybe UTCTime
                                 }
                   
data Faucet s = Faucet (MVar (FaucetState s)) (MVar FaucetCommand)

initFaucet :: StateT s IO () -> IO (Faucet s)
initFaucet sFun= do
  cmdM <- newEmptyMVar
  startTime <- getCurrentTime
  stM <- newMVar $  FaucetState  sFun False startTime Nothing  Nothing
  return $ Faucet stM cmdM

runFaucet :: Faucet s -> IO ()
runFaucet (Faucet stateMVar cmdMVar) = do
  (FaucetState fFun fRun _ _ _) <- takeMVar stateMVar
  case fRun of
    False -> return ()
    True -> do
      tStart <- getCurrentTime
      
             
  


main :: IO ()
main = undefined
  