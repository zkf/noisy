module Data.BanditSolver.OBFinder where

import Data.BanditSolver.LTS
import System.Random.Mersenne.Pure64
import Control.Monad.State
import Data.List (sort)
import qualified Data.Vector as V
import Statistics.Sample (meanVarianceUnb)
    
-- testing
muBestArm = 5.0
sigmaBestArm = 2.0
numArms = 2
muStartEstimate = muBestArm * 2.0
sigmaStartEstimate = muStartEstimate / 20.0
realArms = V.fromList $ (muBestArm, sigmaBestArm) : replicate (numArms-1) (4.0, 4.0)
startEstimates = V.fromList $ replicate numArms (muStartEstimate, sigmaStartEstimate)
rounds = 1000
obnoises = [0.01,0.02..1.0]
numActions = length obnoises
env = Env (realArms, startEstimates, rounds, obnoises)

instance Environment Env where
    getReward (Env (realArms, startEstimates, rounds, actions)) index = do
        let ob = actions !! index
        runAvg realArms 100 rounds (makeLTS startEstimates ob)

obEstMu = fromIntegral rounds * muBestArm * 2
obEstSigma = obEstMu / 20.0
startEstimateOB = (obEstMu, obEstSigma)
startEstimatesOB = V.fromList $ replicate numActions startEstimateOB -- test ob from 0 to 9 

-- findAvgOB :: PureMT -> (Double, Double)
-- findAvgOB g =
--     let obs = evalState (replicateM 10 findOB) g
--         (mean, var) = meanVarianceUnb $ V.fromList obs
--     in (mean, sqrt var)

--findOB :: State PureMT Double
findOB = do
    (LTS (arms, _, _)) <- runOne env 10000 (makeLTS startEstimatesOB 100)
    let bestOB = map (\((m, s), action) -> (action, m, s))
                     . take 10 . reverse
                     . sort $ zip (V.toList arms) obnoises
    return bestOB

newtype Env = Env (GaussianArms -- real arms
                  ,GaussianArms -- starting estimates
                  ,Int          -- number of rounds
                  ,[Double] )   -- obnoises to test


