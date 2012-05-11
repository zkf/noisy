{-# LANGUAGE BangPatterns #-}
module Data.BanditSolver.UCB1
(
runAveragedInstantRewards 
)
where

import Data.BanditSolver.BanditSolver
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Vector.Generic.Mutable (write)
import Data.List.Extras (argmax)
import System.Random.Mersenne.Pure64
import Control.Monad.State
import Control.Monad.Writer

data UCB1 = UCB1 
                !(V.Vector Reward)  -- cumulative rewards
                !(V.Vector Int)     -- number of pulls
                Bool                -- Are we in the init phase?

makeUCB1 :: Int -> UCB1
makeUCB1 numArms =
    let counts = V.replicate numArms 0
        rewards = V.replicate numArms 0
        !ucb1 = UCB1 rewards counts True
    in ucb1

instance Solver UCB1 where
    {-# INLINE select #-}
    select (UCB1 rewards counts initPhase) = 
        if not initPhase then do
            let fun j = avg j + sqrt (2 * log count / fromIntegral (counts ! j))
                avg j = (rewards ! j) / fromIntegral (counts ! j)
                count = fromIntegral $ V.sum counts
            return $ argmax fun [0..V.length counts - 1]
           else do
               let index = V.findIndex (< 1) counts
               case index of
                   Nothing -> select (UCB1 rewards counts False)
                   Just i -> return i

    {-# INLINE update #-}
    update (UCB1 rewards counts initPhase) index reward =
        let !r = rewards ! index + reward
            !rewards' = V.modify (\v -> write v index r) rewards
            !c = counts ! index + 1
            !counts' = V.modify (\v -> write v index c) counts
        in (UCB1 rewards' counts' initPhase)

    getCumulativeReward (UCB1 rewards _ _) = V.sum rewards

runAveragedInstantRewards :: GA -> GA -> Int
    -> Int -> Int -> PureMT -> [String]
runAveragedInstantRewards bestArm badArm numArms rounds repetitions gen =
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agents = replicate repetitions (makeUCB1 numArms)
    in  evalState (execWriterT $ runInstantRewards realArms agents rounds) gen

