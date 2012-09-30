{-# LANGUAGE BangPatterns #-}
module Data.BanditSolver.UCB1
( runAveragedInstantRewards 
, runAverageCumulativeReward 
, makeUCB1
, UCB1(..)
)
where

import Data.BanditSolver.BanditSolver
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Vector.Generic.Mutable (write)
import Data.List.Extras (argmax)
import System.Random.Mersenne.Pure64
import Control.Monad.State
import Data.Random (MonadRandom)
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan.Strict
import Text.Printf

data UCB1 = UCB1 
                !(V.Vector Reward)  -- cumulative rewards
                !(V.Vector Reward)  -- sqrd. cumulative rewards
                !(V.Vector Int)     -- number of pulls
                Bool                -- Are we in the init phase?

makeUCB1 :: Int -> UCB1
makeUCB1 numArms =
    let counts = V.replicate numArms 0
        rewards = V.replicate numArms 0
        rewards2 = V.replicate numArms 0
        !ucb1 = UCB1 rewards rewards2 counts True
    in ucb1

{-# INLINE var #-}
var :: V.Vector Double -> V.Vector Double -> V.Vector Int -> Int -> Double
var  r r2 n i =
    let ns = fromIntegral (n ! i)
    in (r2 ! i) / ns - ((r ! i) / ns)^(2::Int)


instance Solver UCB1 where
    {-# SPECIALIZE INLINE select :: UCB1 -> RandomState ActionId #-}
    {-# INLINE select #-}
    select (UCB1 rewards rewards2 counts initPhase) = 
        if not initPhase then do
            let fun j = avg j + sqrt (logTerm j * min (1/4) (v j))
                v   j = var rewards rewards2 counts j + sqrt (2 * logTerm j)
                logTerm j = log count / n j
                n j   = fromIntegral $ counts ! j
                avg j = (rewards ! j) / n j
                count = fromIntegral $ V.sum counts
            return $ argmax fun [0..V.length counts - 1]
           else do
               let index = V.findIndex (< 1) counts
               case index of
                   Nothing -> select (UCB1 rewards rewards2 counts False)
                   Just i -> return i

    {-# INLINE update #-}
    update (UCB1 rewards rewards2 counts initPhase) index reward =
        let !r = rewards ! index + reward
            !rewards' = V.modify (\v -> write v index r) rewards
            !r2 = rewards2 ! index + reward^(2::Int)
            !rewards2' = V.modify (\v -> write v index r2) rewards2
            !c = counts ! index + 1
            !counts' = V.modify (\v -> write v index c) counts
        in (UCB1 rewards' rewards2' counts' initPhase)

    getCumulativeReward (UCB1 rewards _ _ _) = V.sum rewards

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
        agents = replicate repetitions (makeUCB1 numArms)
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
        solver = makeUCB1 numArms
    res <- runAvg realArms repetitions rounds solver
    return $ show numArms ++ " " ++ show res

