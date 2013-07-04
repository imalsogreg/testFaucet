module Main where

import Control.Concurrent
import Control.Applicative
import Data.Time
import Control.Monad.State

data FaucetCommand = FaucetStart | FaucetStop | FaucetDestroy (MVar ())

data FaucetState s = FaucetState { faucetFun :: StateT s IO ()
                                 , faucetInitTime     :: UTCTime
                                 , faucetLastRunStart :: Maybe UTCTime
                                 , faucetLastRunEnd   :: Maybe UTCTime
                                 }
                   
data Faucet s = Faucet (MVar (FaucetState s)) (MVar FaucetCommand)

initFaucet :: IO (Faucet s)
initFaucet = do
  cmdM <- newEmptyMVar
  startTime <- getCurrentTime
  stM <- newMVar $  FaucetState  sFun startTime Nothing  Nothing
  return $ Faucet stM cmdM
    where
      sFun :: s -> IO (a,s)
      sFun = 


main :: IO ()
main = undefined
  