module Data.BanditSolver.OBFinder (findOB) where

import Data.BanditSolver.LTS
import Data.List (sortBy)
import Control.Monad.State
import System.Random.Mersenne.Pure64
import qualified Data.Vector as V
    
-- testing
-- muStartEstimate = muBestArm * 2.0
-- sigmaStartEstimate = muStartEstimate / 20.0
obFinderRounds :: Int
obFinderRounds = 10000

findOB :: Int -> GA -> GA -> GA -> Int
    -> State PureMT [(Double, Double, Double)]
findOB rounds bestArm badArm armEstimate numArms = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        actions = [0.01,0.02..5.0] :: [Double] -- Observation noises to choose from
        numActions = length actions
        (bestMu, _) = bestArm
        obEstMu          = fromIntegral rounds * 2 * bestMu
                                 -- Twice the expected reward from best arm
        obEstSigma       = obEstMu / 80.0
        solver = makeLTS (GaussianArm obEstMu obEstSigma) numActions 5.0
        env = makeEnv rounds myBestArm myBadArm myArmEstimate numArms actions
    (LTS arms _ _) <- runOne env obFinderRounds solver
    let zipFun (GaussianArm m s) ob = (ob, m, s)
        leastSigma (_, _, s1) (_, _, s2) = s1 `compare` s2
        -- ppObnoises = map fromRational actions :: [Double]
        bestOB n = take n
                     . sortBy leastSigma 
                     $ zipWith zipFun (V.toList arms) actions
    return $ bestOB 10

makeEnv :: Int -> GaussianArm -> GaussianArm -> GaussianArm -> Int
    -> [Double] -> Env
makeEnv rounds bestArm badArm armEstimate numArms obnoises =
    let realArms = V.fromList $ bestArm : replicate (numArms - 1) badArm
        startEstimates = V.fromList $ replicate numArms armEstimate
    in Env (realArms, startEstimates, rounds, obnoises)
    

newtype Env = Env (GaussianArms -- real arms
                  ,GaussianArms -- starting estimates
                  ,Int          -- number of rounds
                  ,[Double] )   -- obnoises to test

instance Environment Env where
    getReward (Env (realArms, startEstimates, rounds, actions)) index = do
        let ob = actions !! index
            solver = LTS startEstimates 0 ob
        runAvg realArms 50 rounds solver
