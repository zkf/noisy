module Data.BanditSolver.LTS (runAveragedLts, GaussianArms(..)) where

import Control.Monad.State 
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT)
import Data.RVar (sampleRVar)
import Statistics.Sample (meanVariance)
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Vector.Generic.Mutable (write)

-- Number of LTSs to take the average of
numLtss :: Int
numLtss = 100
    
instance Arms GaussianArms where

    getReward (GaussianArms arms) idx = gaussian (arms ! idx)
    
    selectArm (GaussianArms arms) = V.maxIndex `fmap` V.mapM gaussian arms

    updateSelectedArm (GaussianArms arms) index ob reward = 
        let (mu, sigma) = arms ! index
            armVariance = sigma**2
            obVariance  = ob**2
            mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
            sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
            arm'   = (mu', sigma')
            arms' =  V.modify (\v -> write v index arm') arms
        in (GaussianArms arms')

gaussian :: GaussianArm -> State PureMT Double
gaussian (mu, sigma) =
    sampleRVar $ normal mu sigma


class Arms as where
    selectArm :: as -> State PureMT Int
    updateSelectedArm :: as -> Int -> ObNoise -> Double -> as
    getReward :: as -> Int -> State PureMT Double
    
newtype (Arms as) => BanditSolver as = BanditSolver (as, CReward)
    
type GaussianArm = (Double, Double) -- Mean, standard deviation
newtype GaussianArms = GaussianArms (V.Vector GaussianArm)
    

type ObNoise = Double
type CReward = Double
type LTS = BanditSolver (GaussianArms, CReward)


runAveragedLts :: (Arms a)  =>
       a    -- Real arms
       -> a -- beginning arm estimates
       -> Int       -- Rounds
       -> ObNoise
       -> PureMT
       -> (Double, Double) -- mean, stddev of cumulative rewards
runAveragedLts arms armEstimates rounds obNoise gen = (mean, sqrt variance)
    where (mean, variance) = runManyLtss arms rounds ltsProto obNoise gen
          ltsProto = (BanditSolver (armEstimates, 0))

runManyLtss :: (Arms a) =>  
    a -> Int -> BanditSolver a -> ObNoise -> PureMT -> (Double, Double) 
runManyLtss realArms rounds lts ob gen = avgReward
    where avgReward = meanVariance . V.fromList . map getCReward $ results
          getCReward (BanditSolver (_, r)) = r
          results = evalState (replicateM numLtss $ runLts realArms rounds ob lts) gen

runLts :: Arms a => a -> Int -> ObNoise -> BanditSolver a -> State PureMT (BanditSolver a)
runLts realArms rounds ob startingLts =
    execStateT (replicateM rounds (pullArm realArms ob)) startingLts
            
pullArm :: Arms a => a -> ObNoise -> StateT (BanditSolver a) (State PureMT) ()
pullArm realArms ob = do
    BanditSolver (arms, cReward) <- get
    selected                     <- lift $ selectArm arms 
    reward                       <- lift $ getReward realArms selected
    let arms' = updateSelectedArm arms selected ob reward 
    put (BanditSolver (arms', cReward + reward))


