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
        actions = V.fromList [0.01,0.02..5.0] :: V.Vector Double -- Observation noises to choose from
        numActions = V.length actions
        obEstMu     = 2.0 -- Twice the scaled expected reward from best arm
        obEstSigma  = 0.1
        obOb        = 0.15
        obSolver = makeLTS (GaussianArm obEstMu obEstSigma) numActions obOb
        env = makeEnv rounds myBestArm myBadArm myArmEstimate numArms actions
    (LTS arms _ _) <- runOne env obFinderRounds obSolver
    let zipFun (GaussianArm m s) ob = (ob, m, s)
        leastSigma (_, _, s1) (_, _, s2) = s1 `compare` s2
        -- ppObnoises = map fromRational actions :: [Double]
        bestOB n = take n
                     . sortBy leastSigma 
                     . V.toList
                     $ V.zipWith zipFun arms actions
    return $ bestOB 10

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
    -- Reward is scaled towards 1.0
    getReward (Env (realArms, startEstimates, rounds, actions)) index = do
        let ob = actions V.! index
            solver = LTS startEstimates 0 ob
            repetitions = 5 * (10000 `div` rounds)
            bestReward = let (GaussianArm m _) = realArms V.! 0 -- the first arm must be the best one
                         in  m * fromIntegral rounds
        unscaledReward <- runAvg realArms repetitions rounds solver
        return $ unscaledReward / bestReward


