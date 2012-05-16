module Data.BanditSolver.OBFinder (findOB, Strategy(..)) where

import Control.Monad.State
import System.Random.Mersenne.Pure64
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.BanditSolver.BanditSolver
import Data.BanditSolver.LTS
import Data.BanditSolver.UCB1
import Text.Printf
import Data.List (sortBy)
import Data.Function (on)

data Strategy = StratLTS | StratUCB1
    
-- testing
-- muStartEstimate = muBestArm * 2.0
-- sigmaStartEstimate = muStartEstimate / 20.0
obFinderRounds :: Int
obFinderRounds = 4000

findOB :: Int -> GA -> GA -> GA -> Int -> Strategy
    -> State PureMT String
findOB rounds bestArm badArm armEstimate numArms strat = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        actions = V.fromList [0.01,0.02..0.30] :: V.Vector Double -- Observation noises to choose from
        numActions = V.length actions
        env = makeEnv rounds myBestArm myBadArm myArmEstimate numArms actions
    bestIndex <- case strat of
                      StratLTS -> useLTS numActions env 
                      StratUCB1 -> useUCB1 numActions env
    let bestOB = actions ! bestIndex
    return $
        unwords [unwords $ map showDouble [fst bestArm, snd bestArm, fst badArm, snd badArm]
               ,show numArms, show rounds, showDouble bestOB]

useLTS :: Int -> Env -> State PureMT Int -- index of best ob
useLTS numActions env = do
    let obEstMu     = 2.0 -- Twice the scaled expected reward from best arm
        obEstSigma  = 2.0
        obOb        = 0.001
        obSolver = makeLTS (GaussianArm obEstMu obEstSigma) numActions obOb
    (LTS arms _ _) <- runOne env obFinderRounds obSolver
    let sigma (GaussianArm _ s) = s
        mu    (GaussianArm m _) = m
        -- biggestMean (_, m1, _) (_, m2, _) = m2 `compare` m1
        bestOBIndex = fst . last . sortBy (compare `on` (mu.snd)) $ zip [0..] (V.toList arms)
    return bestOBIndex

useUCB1 :: Int -> Env -> State PureMT Int --index of best ob
useUCB1 numActions env =  do
    let obSolver      = makeUCB1 numActions
    (UCB1 rewards _ counts _) <- runOne env obFinderRounds obSolver
    let means = V.toList $ V.zipWith (\r c -> r / fromIntegral c) rewards counts
    let bestOBIndex = fst . last . sortBy (compare `on` snd) $ zip [0..] means
    return bestOBIndex



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
    -- Reward is scaled so that mean best reward == 1.0.
    {-# SPECIALIZE INLINE getReward :: Env -> ActionId -> RandomState Reward #-}
    getReward (Env (realArms, startEstimates, rounds, actions)) index = do
        let ob = actions ! index
            solver = LTS startEstimates 0 ob
            repetitions = max 100 (100000 `div` rounds)
            bestMean = gm
                where  (GaussianArm gm _) = realArms ! 0 -- the first arm must be the best one
        unscaledReward <- runAvg realArms repetitions rounds solver
        return $ unscaledReward / (fromIntegral rounds * bestMean) 


