{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Data.BanditSolver.LTS (runAveragedLTS, runOne, Environment(..),
    updateLTS, LTS(..), makeLTS, runAvg, Solver(..), GaussianArms, GaussianArm)
where
import Control.Monad.Writer
import Control.Monad.State
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT)
import Data.RVar (sampleRVar)
import Statistics.Sample (mean, meanVarianceUnb)
import Data.Vector.Generic.Mutable (write)
import Control.Parallel (pseq)
import Data.Vector ((!))
import qualified Data.Vector as V

type ActionId = Int
type Reward   = Double
type GaussianArm = (Double, Double) -- Mean, standard deviation
type GaussianArms = V.Vector GaussianArm
data LTS = LTS !(GaussianArms, Double, Double) deriving (Show)-- arms, cumulative reward, obnoise

makeLTS :: GaussianArms -> Double -> LTS
makeLTS startEstimates ob = LTS (startEstimates, 0, ob)

class Environment e where
    getReward :: e -> ActionId -> State PureMT Double
    
class Solver s where
    select :: s -> State PureMT ActionId
    update :: s -> ActionId -> Reward -> s
    continue :: s -> Bool -- condition on which to stop early
    getArms :: s -> GaussianArms
    getCumulativeReward :: s -> Double
    select = selectArm
    continue _ = True

instance Environment GaussianArms where 
    getReward arms idx = gaussian (arms ! idx)

instance Solver LTS where
    update = updateLTS
    getArms (LTS (arms, _, _)) = arms
    getCumulativeReward (LTS (_, r, _)) = r


runOne :: (Environment e, Solver s) => e -> Int -> s -> State PureMT s
runOne realArms rounds startSolver = run 0 startSolver
    where run n !solver 
            | n == rounds = return solver
            | otherwise   = do
                solver' <- oneRound realArms solver
                if continue solver' 
                    then run (n+1) solver'
                    else return solver'

runAvg :: (Environment e, Solver s) => e -> Int -> Int -> s -> State PureMT Double
runAvg realArms n rounds startSolver = do
    res <- mapM (runOne realArms rounds) (replicate n startSolver)
    return . mean . V.fromList $ map getCumulativeReward res


runAveragedLTS :: GaussianArms -> GaussianArms -> Double -> Int -> Int -> PureMT
    -> [(Int, Double, Double, Double)] -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTS realArms startEstimates ob rounds repetitions randomgen = 
    let result = snd $ evalState (runWriterT $ runEnsemble realArms rounds solvers) randomgen
    in force result `seq` result
  where solvers = replicate repetitions (LTS (startEstimates, 0, ob))
        force ((!a,!b,!c,!d):xs) = (a,b,c,d) `pseq` force xs
        force []                 = ()

runEnsemble :: GaussianArms -> Int -> [LTS] ->
    WriterT [(Int, Double, Double, Double)] (State PureMT) ()
runEnsemble realArms rounds startSolvers = run 0 startSolvers
  where run n solvers
            | n `notElem` checkpoints = do
                solvers' <- lift $ oneRoundEnsemble realArms solvers
                force solvers' `seq` run (n+1) solvers'
            | otherwise = do
                solvers' <- lift $ oneRoundEnsemble realArms solvers
                let crewards = force solvers' `seq` map getCumulativeReward solvers'
                    (mean, variance) = meanVarianceUnb $ V.fromList crewards
                    ob = getOb $ head solvers'
                tell [(n, ob, mean, sqrt variance)]
                unless (n == rounds) $ run (n + 1) solvers'
        getOb (LTS (_, _, ob))     = ob
        checkpoints = rounds : [y * 10^x | y <- [1, 5],
              x <- [1 :: Int .. floor (logBase 10 $
                                         fromIntegral rounds/5.0 :: Double)]]
        force (x:xs) = x `pseq` force xs
        force []     = ()

oneRoundEnsemble :: GaussianArms -> [LTS] -> State PureMT [LTS]
oneRoundEnsemble realArms solvers = mapM (oneRound realArms) solvers
   

oneRound :: (Environment e, Solver s) => e -> s -> State PureMT s
oneRound env solver = do
    selected <- select solver
    reward   <- getReward env selected
    let !solver' = update solver selected reward
    return solver'

selectArm :: Solver s => s -> State PureMT ActionId
selectArm solver = do
    let arms = getArms solver
    V.maxIndex `fmap` V.mapM gaussian arms

updateLTS :: LTS -> ActionId -> Reward -> LTS
updateLTS (LTS (arms, creward, ob)) !index !reward = 
    let (mu, sigma) = arms ! index
        armVariance = sigma**2
        obVariance  = ob**2
        !mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        !sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        !arms' =  V.modify (\v -> write v index (mu', sigma')) arms
        !creward' = creward + reward
    in (LTS (arms', creward', ob))

gaussian :: GaussianArm -> State PureMT Double
gaussian (mu, sigma) =
    sampleRVar $ normal mu sigma
