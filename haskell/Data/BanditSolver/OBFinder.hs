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

findOB :: Int -> [GA] -> GA -> Strategy
    -> State PureMT String
findOB rounds arms armEstimate strat = do
    let myArmEstimate = makeGaussianArm armEstimate
        actions = V.fromList (0.01: [0.1, 0.2 .. 5.0]) :: V.Vector Double -- Observation noises to choose from
        numActions = V.length actions
        env = makeEnv rounds arms myArmEstimate actions
    bestIndex <- case strat of
                      StratLTS -> useLTS numActions env 
                      StratUCB1 -> useUCB1 numActions env
    let bestOB = actions ! bestIndex
        start = max 0 (bestOB - 0.15)
        end   = bestOB + 0.15
        actions2 = V.fromList [start, start + 0.01 .. end] :: V.Vector Double
        numActions2 = V.length actions2
        env2 = makeEnv rounds arms myArmEstimate actions2
    bestIndex2 <- case strat of
                      StratLTS -> useLTS numActions2 env2
                      StratUCB1 -> useUCB1 numActions2 env2
    let bestOB2 = actions2 ! bestIndex2
        (bestArm:badArm:_) = arms
    return $
        unwords [unwords $ map showDouble [fst bestArm, snd bestArm, fst badArm, snd badArm]
               ,show (length arms), show rounds, showDouble bestOB2]

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


makeEnv :: Int -> [(Double, Double)] -> GaussianArm -> V.Vector Double -> Env
makeEnv rounds arms armEstimate obnoises =
    let startEstimates = V.fromList $ replicate (length arms) armEstimate
        realArms = V.fromList $ map (uncurry GaussianArm) arms
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


