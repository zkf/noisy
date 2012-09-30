module Data.BanditSolver.Null
( runAveragedInstantRewards 
, runAverageCumulativeReward
, makeNull
, Null(..)
)
where

import Data.BanditSolver.BanditSolver
import qualified Data.Vector as V
import System.Random.Mersenne.Pure64
import Control.Monad.State
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan.Strict
import Text.Printf
import Data.RVar (sampleRVar)
import Data.Random (MonadRandom, uniform)

data Null = Null !Int !Reward -- numArms, Cumulative Reward

makeNull :: Int -> Null
makeNull numArms = Null numArms 0.0

instance Solver Null where
    {-# SPECIALIZE INLINE select :: Null -> RandomState ActionId #-}
    {-# INLINE select #-}
    select (Null numArms _) = sampleRVar (uniform 0 (numArms - 1))

    {-# INLINE update #-}
    update (Null numArms creward) _ reward = Null numArms (creward + reward)

    getCumulativeReward (Null _ creward) = creward

showDouble :: Double -> String
showDouble = printf "%f" 

evenlyDistribute :: Int -> Int -> [Int]
evenlyDistribute tasks threads = 
    let (chunk, rest) = tasks `divMod` threads
    in  zipWith (+) (replicate threads chunk) (replicate rest 1 ++ repeat 0)

runAveragedInstantRewards :: GA -> GA -> Int
    -> Int -> Int -> IO [String] -- Checkpoint, mean of instant rewards
runAveragedInstantRewards bestArm badArm numArms rounds repetitions = do
    threads <- getNumCapabilities
    let chunks = evenlyDistribute repetitions threads
    chans <- replicateM threads newChan
    _ <- zipWithM (\chan reps -> forkIO $ runAveragedInstantRewardsIO bestArm badArm numArms rounds reps chan) chans chunks
    forM (makeCheckpoints rounds) $ \n -> do
        results <- mapM readChan chans
        let m = sum results / fromIntegral repetitions
        return $ unwords [show n, showDouble m]


runAveragedInstantRewardsIO :: GA -> GA -> Int
    -> Int -> Int -> Chan Double -> IO ()
runAveragedInstantRewardsIO bestArm badArm numArms rounds repetitions chan = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agents = replicate repetitions (makeNull numArms)
    gen <- newPureMT
    evalStateT (runInstantRewards realArms agents rounds chan) gen


{-# SPECIALIZE runAverageCumulativeReward :: 
    GA -> GA -> Int -> Int -> Int -> RandomStateIO String #-}
runAverageCumulativeReward :: (MonadRandom m) =>
      GA -> GA -> Int -> Int -> Int -> m String
runAverageCumulativeReward bestArm badArm numArms rounds repetitions = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        solver = makeNull numArms
    res <- runAvg realArms repetitions rounds solver
    return $ show numArms ++ " " ++ show res

