module Data.BanditSolver.OBFinder where

import Data.BanditSolver.LTS
import Data.List (sortBy)
import qualified Data.Vector as V
    
-- testing
muBestArm = 5.0
sigmaBestArm = 2.0
numArms = 2
muStartEstimate = muBestArm * 2.0
sigmaStartEstimate = muStartEstimate / 20.0
realArms = V.fromList $ (muBestArm, sigmaBestArm) : replicate (numArms-1) (4.0, 4.0)
startEstimates = V.fromList $ replicate numArms (muStartEstimate, sigmaStartEstimate)
rounds = 1000
obnoises = [0.01,0.02..5.0] :: [Rational]
numActions = length obnoises
env = Env (realArms, startEstimates, rounds, obnoises)

obEstMu          = fromIntegral rounds * muBestArm * 2
obEstSigma       = obEstMu / 80.0
startEstimateOB  = (obEstMu, obEstSigma)
startEstimatesOB = V.fromList $ replicate numActions startEstimateOB

findOB = do
    (LTS (arms, _, _)) <- runOne env 10000 (makeLTS startEstimatesOB 5.0)
    let zipFun (m,s) ob = (ob, m, s)
        leastSigma (_, _, s1) (_, _, s2) = s1 `compare` s2
        ppObnoises = map fromRational obnoises :: [Double]
        bestOB n = take n
                     . sortBy leastSigma 
                     $ zipWith zipFun (V.toList arms) ppObnoises
    return $ bestOB 10


newtype Env = Env (GaussianArms -- real arms
                  ,GaussianArms -- starting estimates
                  ,Int          -- number of rounds
                  ,[Rational] )   -- obnoises to test

instance Environment Env where
    getReward (Env (realArms, startEstimates, rounds, actions)) index = do
        let ob = actions !! index
        runAvg realArms 50 rounds (makeLTS startEstimates ob)
