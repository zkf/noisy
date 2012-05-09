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
                (V.Vector Reward)  -- cumulative rewards
                (V.Vector Int)     -- number of pulls

makeUCB1 :: (Environment e) => e -> Int -> State PureMT UCB1
makeUCB1 env numArms = do
    let counts = V.replicate numArms 1
    rewards <- V.mapM (getReward env) (V.enumFromN 0 numArms)
    return $ UCB1 rewards counts

instance Solver UCB1 where
    select (UCB1 rewards counts) = do
        let fun j = avg j + sqrt (2 * log count / fromIntegral (counts ! j))
            avg j = (rewards ! j) / fromIntegral (counts ! j)
            count = fromIntegral $ V.sum counts
        return $ argmax fun [0..V.length counts - 1]

    update (UCB1 rewards counts) index reward =
        let r = rewards ! index + reward
            rewards' = V.modify (\v -> write v index r) rewards
            c = counts ! index + 1
            counts' = V.modify (\v -> write v index c) counts
        in (UCB1 rewards' counts')

    getCumulativeReward (UCB1 rewards _) = V.sum rewards

runAveragedInstantRewards :: GA -> GA -> Int
    -> Int -> Int -> PureMT -> [(Int, Reward, Double)]
runAveragedInstantRewards bestArm badArm numArms rounds repetitions gen =
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        (agents, gen') = runState (replicateM repetitions (makeUCB1 realArms numArms)) gen 
    in  evalState (execWriterT $ runInstantRewards realArms agents rounds) gen'

