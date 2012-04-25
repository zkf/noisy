module Data.BanditSolver.OBFinder where

import Data.BanditSolver.LTS
import System.Random.Mersenne.Pure64
import Control.Monad.State
import Data.List (sort)
import qualified Data.Vector as V
    
-- testing
realArms = V.fromList $ [(5.0, 2.0)] ++ replicate 9 (3.0, 2.0)
startEstimates = V.fromList $ replicate (V.length realArms) (2, 2)
rounds = 1000
env = Env (realArms, startEstimates, rounds)

startEstimatesOB = V.fromList $ replicate 10 (4000, 200) -- test ob from 0 to 9 
test g = 
    let (LTS (arms, _, _)) = evalState (runOneLTS env startEstimatesOB 800 1000) g
        sortedArms = map (\((m, s), rank) -> (rank, m, s)) . take 4 . reverse . sort $ zip (V.toList arms) [0..]
    in sortedArms

newtype Env = Env (GaussianArms -- real arms
                  ,GaussianArms -- starting estimates
                  ,Int          -- number of rounds
                  )

instance Environment Env where
    getReward (Env (realArms, startEstimates, rounds)) index =
        let ob = if index == 0 then 0.1 else fromIntegral index
        in runOneLTS realArms startEstimates ob rounds >>= \(LTS (_, reward, _)) -> return reward

