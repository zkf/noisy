{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Main where
import Data.Word
import Foreign.Storable (sizeOf)
import OpenSSL.Random
import Data.Binary.Strict.Get
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64 
import Control.Monad 
import Data.List
import Data.BanditSolver.LTS
import Control.Parallel.Strategies
import qualified Data.Vector as V 
import Control.Applicative
import Data.List.Split
import Control.Monad.State


main :: IO ()
main = do
    (myArmEstimates, myArms, 
        myObNoiseRange, myRounds, myRepetitions) <- (getParams . parse) `fmap` getArgs
    gens <- (map pureMT) `fmap` replicateM (length myObNoiseRange) getOpenSSLRand
    let results = parMap rseq id .
            getZipList $ runSimulation myArms myArmEstimates myRounds myRepetitions <$> 
                ZipList myObNoiseRange <*> ZipList gens
    mapM_ (mapM_ print) (transpose results)
    
runSimulation ::
    GaussianArms -> 
    GaussianArms ->
    Int -> 
    Int ->
    Double -> 
    PureMT -> 
    [(Int, Double, Double, Double)] -- ob, roundN, mean ,stddev
runSimulation arms armEstimates myRounds reps ob rndGen =
    let !resultsList = runAveragedLts 
           arms armEstimates myRounds reps ob rndGen
    in resultsList

-- Get a decent random seed
-- randBytes is not thread-safe!
getOpenSSLRand :: IO Word64
getOpenSSLRand = do
    bytes <- randBytes n 
    let (Right w64, _) = runGet getWord64host bytes
    return w64
  where n = sizeOf (undefined :: Word64)



data Args = Args {
          obStart :: Maybe Double
        , obEnd   :: Maybe Double
        , obStep  :: Maybe Double
         
        , rounds  :: Maybe Int
        , repetitions :: Maybe Int
         
        , bestArm :: Maybe (Double, Double)
        , badArm  :: Maybe  (Double, Double)
        , armEstimate  :: Maybe  (Double, Double)
         
        , numArms  :: Maybe Int
    }

parse :: [String] -> Args 
parse s = (flip execState) nothingArgs $ do
    let args = map words $ wordsBy (=='-') $ unwords s
    mapM_ go args
  where
    go [opt, arg] = do
        maybeArgs <- get
        put $ case opt of
                   "obStart"     -> maybeArgs { obStart     = Just (read arg) }
                   "obEnd"       -> maybeArgs { obEnd       = Just (read arg) }
                   "obStep"      -> maybeArgs { obStep      = Just (read arg) }
                   "rounds"      -> maybeArgs { rounds      = Just (read arg) }
                   "repetitions" -> maybeArgs { repetitions = Just (read arg) }
                   "bestArm"     -> maybeArgs { bestArm     = Just (read arg) }
                   "badArm"      -> maybeArgs { badArm      = Just (read arg) }
                   "armEstimate" -> maybeArgs { armEstimate = Just (read arg) }
                   "numArms"     -> maybeArgs { numArms     = Just (read arg) }
                   _         -> error $ "Invalid option " ++ unwords [opt, arg]
    go x = error $ "Invalid input " ++ unwords x
                   
nothingArgs :: Args
nothingArgs = Args {
          obStart = Nothing
        , obEnd   = Nothing
        , obStep  = Nothing
         
        , rounds  = Nothing
        , repetitions = Nothing
         
        , bestArm = Nothing
        , badArm  = Nothing
        , armEstimate = Nothing
         
        , numArms = Nothing
    }
getParams :: Args -> 
    (GaussianArms, GaussianArms, [Double], Int, Int)
getParams args = 
    let myBestArm = case bestArm args of
                         Just a  -> a
                         Nothing -> error "Missing -bestArm"
        myOtherArms = case badArm args of
                           Just a  -> replicate (myNumArms - 1) a
                           Nothing -> error "Missing -badArm"
        myNumArms = case numArms args of 
                         Just a  -> a
                         Nothing -> error "Missing -numArms"
        myArmEstimate = case armEstimate args of
                             Just a  -> a
                             Nothing -> error "Missing -armEstimate"
        myRounds =  case rounds args of
                         Just a  -> if a > 100000 
                                       then error $ "More than 100000 rounds"
                                                    ++ "are not supported"
                                       else a
                         Nothing -> error "Missing -rounds"
        myRepetitions = case repetitions args of
                             Just a  -> a
                             Nothing -> error "Missing -repetitions"
        myObStart = case obStart args of
                         Just  a -> a
                         Nothing -> error "Missing -obStart"
        myObEnd = case obEnd args of
                       Just  a -> a
                       Nothing -> error "Missing -obEnd"
        myObStep = case obStep args of
                        Just a  -> a
                        Nothing -> error "Missing -obStep"
        
        myArmEstimates = GaussianArms . V.fromList $ replicate myNumArms myArmEstimate
        myArms = GaussianArms . V.fromList $ myBestArm : myOtherArms
        myObNoiseRange = [myObStart, (myObStart + myObStep) .. myObEnd]
    in (myArmEstimates, myArms, myObNoiseRange, myRounds, myRepetitions)
    
