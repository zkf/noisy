{-# LANGUAGE BangPatterns #-}
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
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.Vector as V 
import Control.Applicative
import Data.List.Split
import Control.Monad.State
import System.Directory
import System.IO
import Text.Printf

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    (myArmEstimates, myArms, myObNoiseRange, myRounds, myRepetitions,
        myBestArm, myBadArm, myNumArms, myObNoiseStart, myObNoiseEnd, 
        myObNoiseStep) <- (getParams . parse) `fmap` getArgs
    gens <- (map pureMT) `fmap` replicateM (length myObNoiseRange) getOpenSSLRand
    let results = parMap rseq id . getZipList $ 
            runSimulation myArms myArmEstimates myRounds myRepetitions <$> 
                ZipList myObNoiseRange <*> ZipList gens
        resultsTr = transpose results -- rows: rounds, columns: ob
    showProgress results
    mapM_ (writeResults myRepetitions myNumArms myBestArm myBadArm 
        myObNoiseStart myObNoiseEnd myObNoiseStep) resultsTr
    
writeResults :: Int -> Int -> GaussianArm -> GaussianArm ->
    Double -> Double -> Double -> [(Int, Double, Double, Double)] -> IO ()
writeResults reps myNumArms best bad obstart obend obstep rlist = do
    let myRounds = head.fst'.unzip4 $ rlist
        dir = (show myNumArms) ++ "-arms/" ++ show myRounds ++ "-rounds"
        file = dir ++ "/" ++ "good-" ++ show best ++ 
            "_bad-" ++ show (myNumArms - 1) ++ "-" ++ show bad ++
            "_ob-" ++ show obstart ++ "-" ++ show obend ++ 
            "-" ++ show obstep ++ "_reps-" ++ show reps ++ ".data"
        fst' (x,_,_,_) = x
        header = "# Rounds: " ++ show myRounds ++ ", repetitions: " ++ show reps
            ++ "\n# Good arm ~ N" ++ show best ++ ", " ++ show (myNumArms - 1)
            ++ " bad arm(s) ~ N" ++ show bad
            ++ "\n# Observation noise range from " ++ show obstart ++ " to "
            ++ show obend ++ " with step size " ++ show obstep
            ++ "\n\n# Observation noise | mean | standard deviation\n\n"
    createDirectoryIfMissing True dir
    writeFile file header
    appendFile file $ unlines (map prettyPrint rlist)
   
prettyPrint :: (Int, Double, Double, Double) -> String
prettyPrint (_, ob, mean, stddev) = 
    unwords . map show $ [ob, mean, stddev]
    
    
runSimulation ::
    GaussianArms -> 
    GaussianArms ->
    Int -> 
    Int ->
    Double -> 
    PureMT -> 
    [(Int, Double, Double, Double)] -- ob, roundN, mean ,stddev
runSimulation arms armEstimates myRounds reps ob rndGen =
    let resultsList = runAveragedLTS 
           arms armEstimates ob myRounds reps rndGen
    in force resultsList `seq` resultsList

force :: [a] -> ()
force (x:xs) = x `pseq` force xs
force []      = ()

-- Get a decent random seed
-- randBytes is not thread-safe!
getOpenSSLRand :: IO Word64
getOpenSSLRand = do
    bytes <- randBytes n 
    let (Right w64, _) = runGet getWord64host bytes
    return w64
  where n = sizeOf (undefined :: Word64)


putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r" ++ s

drawProgressBar :: Int -> Rational -> String
drawProgressBar width progress = 
    "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
    where bars   = round (progress * fromIntegral width)
          spaces = width - bars
          
drawPercentage :: Rational -> String
drawPercentage progress = printf "%3d%%" ( round (progress * 100) :: Int )

showProgress :: [a] -> IO ()
showProgress xs = do
    let len = fromIntegral $ length xs
    (flip evalStateT) 0 $ forM_ xs $ \e -> e `seq` do
        progress <- (/len) `fmap` get
        lift $ putProgress $ drawProgressBar 80 progress ++ " " ++ drawPercentage progress
        modify (+1)
    hPutStrLn stderr " Done." 

data Args = Args {
          obStart :: Maybe Double
        , obEnd   :: Maybe Double
        , obStep  :: Maybe Double
         
        , rounds  :: Maybe Int
        , repetitions :: Maybe Int
         
        , bestArm :: Maybe GaussianArm
        , badArm  :: Maybe GaussianArm
        , armEstimate  :: Maybe GaussianArm
         
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
    
getParams :: Args -> (GaussianArms, GaussianArms, [Double], Int, Int,
    GaussianArm, GaussianArm, Int, Double, Double, Double)
getParams args = 
    let myBestArm = case bestArm args of
                         Just a  -> a
                         Nothing -> error "Missing -bestArm"
        myBadArm  = case badArm args of
                         Just a -> a
                         Nothing -> error "Missing -badArm"
        myOtherArms = replicate (myNumArms - 1) myBadArm
        myNumArms = case numArms args of 
                         Just a  -> a
                         Nothing -> error "Missing -numArms"
        myArmEstimate = case armEstimate args of
                             Just a  -> a
                             Nothing -> error "Missing -armEstimate"
        myRounds =  case rounds args of
                         Just a  -> a
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
        
        myArmEstimates = V.fromList $ replicate myNumArms myArmEstimate
        myArms = V.fromList $ myBestArm : myOtherArms
        myObNoiseRange = [myObStart, (myObStart + myObStep) .. myObEnd]
    in (myArmEstimates, myArms, myObNoiseRange, myRounds, myRepetitions,
         myBestArm, myBadArm, myNumArms, myObStart, myObEnd, myObStep)
    
