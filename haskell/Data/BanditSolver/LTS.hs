{-# LANGUAGE BangPatterns #-}
module Data.BanditSolver.LTS (runAveragedLTS, GaussianArms, GaussianArm) where
import Control.Monad.Writer
import Control.Monad.State
import Data.Random (normal)
import System.Random.Mersenne.Pure64 (PureMT)
import Data.RVar (sampleRVar)
import Statistics.Sample (meanVarianceUnb)
import Data.Vector.Generic.Mutable (write)
import Control.Parallel (pseq)
import Data.Vector ((!))
import qualified Data.Vector as V


type GaussianArm = (Double, Double) -- Mean, standard deviation
type GaussianArms = V.Vector GaussianArm
data Solver = Solver !(GaussianArms, Double)


runAveragedLTS :: GaussianArms -> GaussianArms -> Double -> Int -> Int -> PureMT
    -> [(Int, Double, Double, Double)] -- Checkpoint, ob, mean and stddev of cumulative reward
runAveragedLTS realArms startEstimates ob rounds repetitions randomgen = 
    let result = snd $ evalState (runWriterT $ runAll realArms ob rounds solvers) randomgen
    in force result `seq` result
  where solvers = replicate repetitions (Solver (startEstimates, 0))
        force ((!a,!b,!c,!d):xs) = (a,b,c,d) `pseq` force xs
        force []                 = ()


runAll :: GaussianArms -> Double -> Int -> [Solver] ->
    WriterT [(Int, Double, Double, Double)] (State PureMT) ()
runAll realArms ob rounds startSolvers = run 0 startSolvers
  where run n solvers
            | n `notElem` checkpoints = do
                solvers' <- lift $ oneRound realArms ob solvers
                force solvers' `seq` run (n+1) solvers'
            | otherwise = do
                solvers' <- lift $ oneRound realArms ob solvers
                let crewards = force solvers' `seq` map getCReward solvers'
                    (mean, variance) = meanVarianceUnb $ V.fromList crewards
                tell [(n, ob, mean, sqrt variance)]
                if n == rounds
                    then return ()
                    else run (n + 1) solvers'
        getCReward (Solver (_, r)) = r
        checkpoints = rounds : [y * 10^x | y <- [1, 5],
              x <- [1 :: Int .. floor (logBase 10 $
                                         (fromIntegral rounds)/5.0 :: Double)]]
        force (x:xs) = x `pseq` force xs
        force []     = ()
             

oneRound :: GaussianArms -> Double -> [Solver] -> State PureMT [Solver]
oneRound realArms ob solvers = mapM (oneLTS realArms ob) solvers

oneLTS :: GaussianArms -> Double -> Solver -> State PureMT Solver
oneLTS realArms ob solver = do
    selected <- selectArm solver
    reward   <- getReward realArms selected
    let !solver' = update ob solver selected reward
    return solver'

getReward :: GaussianArms -> Int -> State PureMT Double
getReward arms idx = gaussian (arms ! idx)

selectArm :: Solver -> State PureMT Int
selectArm (Solver (arms, _)) =
    V.maxIndex `fmap` V.mapM gaussian arms 

update :: Double -> Solver -> Int -> Double -> Solver
update ob (Solver (arms, creward)) !index !reward = 
    let (mu, sigma) = arms ! index
        armVariance = sigma**2
        obVariance  = ob**2
        !mu' = (armVariance * reward + obVariance * mu)/(armVariance + obVariance)
        !sigma' = sqrt $ (armVariance * obVariance)/(armVariance + obVariance)
        !arms' =  V.modify (\v -> write v index (mu', sigma')) arms
        !creward' = creward + reward
    in (Solver (arms', creward'))

gaussian :: GaussianArm -> State PureMT Double
gaussian (mu, sigma) =
    sampleRVar $ normal mu sigma
