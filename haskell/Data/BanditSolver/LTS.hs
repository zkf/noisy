{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Data.BanditSolver.LTS 

(   GaussianArms, GaussianArm(..), LTS(..), GA,
    runAveragedLTS, runAveragedInstantRewards,
    updateLTS, makeLTS,
    makeGaussianArm
) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Chan.Strict
import Control.Monad.State
import Control.Monad.Writer
import Data.RVar (sampleRVar)
import Data.Random (MonadRandom)
import Data.Random (normal)
import Data.Vector ((!))
import Data.Vector.Generic.Mutable (write)
import System.Random.Mersenne.Pure64 (PureMT, newPureMT)
import qualified Data.Vector as V
import Data.BanditSolver.BanditSolver

data GaussianArm = GaussianArm !Double !Double deriving (Show)-- Mean, standard deviation
type GaussianArms = V.Vector GaussianArm
data LTS = LTS !GaussianArms !Double !Double deriving (Show)-- arms, cumulative reward, obnoise

type GA = (Double, Double)
makeGaussianArm :: GA -> GaussianArm
makeGaussianArm (mu, sigma) = GaussianArm mu sigma

makeLTS :: GaussianArm -> Int -> Double -> LTS
makeLTS armEstimate numArms ob = LTS startEstimates 0 ob
  where startEstimates = V.fromList $ replicate numArms armEstimate


instance Environment GaussianArms where 
    {-# SPECIALIZE INLINE getReward :: GaussianArms -> ActionId -> RandomStateIO Reward #-}
    {-# SPECIALIZE INLINE getReward :: GaussianArms -> ActionId -> RandomState Reward #-}
    {-# INLINE getReward #-}
    getReward arms idx = gaussian (arms ! idx)

instance Solver LTS where
    {-# INLINE select #-}
    select = selectArm
    {-# INLINE update #-}
    update = updateLTS
    getCumulativeReward (LTS _ r _) = r

{-# SPECIALIZE INLINE oneRound :: GaussianArms -> LTS -> RandomStateIO (LTS, Reward) #-}
{-# SPECIALIZE INLINE oneRound :: GaussianArms -> LTS -> RandomState (LTS, Reward) #-}


runAveragedLTS :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> WriterT [(Int, Double, Double)] IO () -- Checkpoint, ob, mean of cumulative reward
runAveragedLTS bestArm badArm armEstimate numArms rounds repetitions ob = do
    threads <- lift getNumCapabilities
    let chunk = repetitions `div` threads
    chans <- lift $ replicateM threads newChan
    _ <- lift $ mapM (forkIO . runAveragedLTSIO bestArm badArm armEstimate numArms rounds chunk ob) chans
    forM_ (makeCheckpoints rounds) $ \n -> do
        results <- liftIO $ mapM readChan chans
        let m = sum results / fromIntegral repetitions
        tell [(n, ob, m)]
    return ()

runAveragedLTSIO :: GA -> GA -> GA ->  Int
    -> Int -> Int -> Double -> Chan Double -> IO () -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTSIO bestArm badArm armEstimate numArms rounds repetitions ob chan = do
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        solvers = replicate repetitions (makeLTS myArmEstimate numArms ob)
    gen <- newPureMT
    evalStateT (runEnsembleIO realArms rounds solvers chan) gen

runAveragedInstantRewards :: GA -> GA -> GA -> Int
    -> Int -> Int -> Double -> PureMT -> [(Int, Reward, Double)]
runAveragedInstantRewards bestArm badArm armEstimate numArms rounds repetitions ob gen =
    let myBestArm = makeGaussianArm bestArm
        myBadArm  = makeGaussianArm badArm
        myArmEstimate = makeGaussianArm armEstimate
        badArms = replicate (numArms - 1) myBadArm
        realArms = V.fromList $ myBestArm : badArms
        agents = replicate repetitions (makeLTS myArmEstimate numArms ob)
    in  evalState (execWriterT $ runInstantRewards realArms agents rounds) gen

{-# SPECIALIZE INLINE selectArm :: LTS -> RandomStateIO ActionId #-}
{-# SPECIALIZE INLINE selectArm :: LTS -> RandomState ActionId #-}
{-# INLINE selectArm #-}
selectArm :: (MonadRandom m) => LTS -> m ActionId
selectArm (LTS arms _ _) = do
   let start = V.length arms - 1
       go 0 maxValue index = do
           newValue <- gaussian (arms ! 0)
           if newValue > maxValue 
               then return 0
               else return index 
       go n maxValue index = do
           newValue <- gaussian (arms ! n)
           if newValue > maxValue 
               then go (n - 1) newValue n
               else go (n - 1) maxValue index
   firstValue <- gaussian (arms ! start)
   go (start - 1) firstValue start

-- Alternative to the `go' function above is this:
--      V.maxIndex `fmap` V.mapM gaussian arms
-- The downside is that it is much slower.


{-# SPECIALIZE INLINE updateLTS :: LTS -> ActionId -> Reward -> LTS #-}
{-# INLINE updateLTS #-}
updateLTS :: LTS -> ActionId -> Reward -> LTS
updateLTS (LTS arms creward ob) !index !reward = 
    let (GaussianArm mu sigma) = arms ! index
        armVariance = sigma * sigma
        obVariance  = ob * ob
        !mu'      = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        !sigma'   = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        !arms'    =  V.modify (\v -> write v index (GaussianArm mu' sigma')) arms
        !creward' = creward + reward
    in LTS arms' creward' ob

{-# SPECIALIZE INLINE gaussian :: GaussianArm -> RandomStateIO Reward #-}
{-# SPECIALIZE INLINE gaussian :: GaussianArm -> RandomState Reward #-}
{-# INLINABLE gaussian #-}
gaussian :: (MonadRandom m) => GaussianArm -> m Reward
gaussian (GaussianArm mu sigma) =
    sampleRVar $ normal mu sigma

