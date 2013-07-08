module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Applicative
import Data.Time
import Control.Monad.State

data FaucetCommand = FaucetStart | FaucetStop | FaucetDestroy (MVar ())

data FaucetState s = FaucetState { faucetFun          :: StateT s IO ()
                                 , faucetRunning      :: Bool
                                 , faucetInitTime     :: UTCTime
                                 , faucetLastRunStart :: Maybe UTCTime
                                 , faucetLastRunEnd   :: Maybe UTCTime
                                 , faucetDelay        :: Int
                                 }
                   
data Faucet s = Faucet (MVar (FaucetState s)) (MVar FaucetCommand)

initFaucet :: StateT s IO () -> IO (Faucet s)
initFaucet sFun= do
  cmdM <- newEmptyMVar
  startTime <- getCurrentTime
  stM <- newMVar $  FaucetState  sFun False startTime Nothing  Nothing 500000
  return $ Faucet stM cmdM

runFaucet :: (Num s) => Faucet s -> IO ()
runFaucet (Faucet stateMVar cmdMVar) = do
  fSt@(FaucetState fFun fRun tInit _ _ d) <- takeMVar stateMVar
  case fRun of
    False -> return ()
    True -> do
      tStart <- getCurrentTime
      let thisData = 1
      (a,s') <- runStateT (faucetFun fSt) $ thisData
      putStrLn $ "Answer is " ++ show a
      tStop <- getCurrentTime
      putMVar stateMVar (FaucetState fFun fRun tInit (Just tStart) (Just tStop) d)
      let f' = Faucet stateMVar cmdMVar
      threadDelay $ faucetDelay fSt
      runFaucet f'
        
--updateState :: StateT s IO () -> f -> d -> StateT IO ()
--updateState st f d =
  


main :: IO ()
main = undefined
  