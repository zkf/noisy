module Data.BanditSolver.OBFinder (findOB, Strategy(..)) where

import Control.Monad.State
import System.Random.Mersenne.Pure64
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.BanditSolver.BanditSolver
import Data.BanditSolver.LTS
import Data.BanditSolver.UCB1
import Text.Printf

data Strategy = StratLTS | StratUCB1
    
-- testing
-- muStartEstimate = muBestArm * 2.0
-- sigmaStartEstimate = muStartEstimate / 20.0
obFinderRounds :: Int
obFinderRounds = 10000

findOB :: Int -> GA -> GA -> GA -> Int -> Strategy
    -> State PureMT String
findOB rounds bestArm badArm armEstimate numArms strat = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        actions = V.fromList [0.01,0.02..5.0] :: V.Vector Double -- Observation noises to choose from
        numActions = V.length actions
        env = makeEnv rounds myBestArm myBadArm myArmEstimate numArms actions
    bestIndex <- case strat of
                      StratLTS -> useLTS numActions env 
                      StratUCB1 -> useUCB1 numActions env
    let bestOB = actions ! bestIndex
    return $
        unwords [unwords $ map showDouble [fst bestArm, snd bestArm, fst badArm, snd badArm]
               ,show numArms, show rounds, showDouble bestOB]

useLTS :: Int -> Env -> State PureMT Int
useLTS numActions env = do
    let obEstMu     = 1.0 -- Twice the scaled expected reward from best arm
        obEstSigma  = 0.5
        obOb        = 0.000003
        obSolver = makeLTS (GaussianArm obEstMu obEstSigma) numActions obOb
    (LTS arms _ _) <- runOne env obFinderRounds obSolver
    let compSigma (GaussianArm _ s1) (GaussianArm _ s2) = s1 `compare` s2
        -- biggestMean (_, m1, _) (_, m2, _) = m2 `compare` m1
        bestOB = V.minIndexBy compSigma arms
    return bestOB

useUCB1 :: Int -> Env -> State PureMT Int
useUCB1 numActions env =  do
    let obSolver      = makeUCB1 numActions
    (UCB1 _ counts _) <- runOne env obFinderRounds obSolver
    let bestOB        = V.maxIndex counts
    return  bestOB



showDouble :: Double -> String
showDouble = printf "%f" 


makeEnv :: Int -> GaussianArm -> GaussianArm -> GaussianArm -> Int
    -> V.Vector Double -> Env
makeEnv rounds bestArm badArm armEstimate numArms obnoises =
    let realArms = V.fromList $ bestArm : replicate (numArms - 1) badArm
        startEstimates = V.fromList $ replicate numArms armEstimate
    in Env (realArms, startEstimates, rounds, obnoises)
    

newtype Env = Env (GaussianArms -- real arms
                  ,GaussianArms -- starting estimates
                  ,Int          -- number of rounds
                  ,V.Vector Double )   -- obnoises to test
                  deriving (Show)

instance Environment Env where
    -- Reward is scaled with (reward - mean reward)/(max reward - min reward)
    {-# SPECIALIZE INLINE getReward :: Env -> ActionId -> RandomState Reward #-}
    getReward (Env (realArms, startEstimates, rounds, actions)) index = do
        let ob = actions ! index
            solver = LTS startEstimates 0 ob
            repetitions = max 50 (5 * (10000 `div` rounds))
            bestReward =
                let (GaussianArm gm _) = realArms ! 0 -- the first arm must be the best one
                in  gm * fromIntegral rounds
            worstReward = 
                let (GaussianArm bm _) = realArms ! 1
                in bm * fromIntegral rounds
            meanReward = (bestReward + worstReward) / 2
        unscaledReward <- runAvg realArms repetitions rounds solver
        return $ (unscaledReward - meanReward) / (bestReward - worstReward)


