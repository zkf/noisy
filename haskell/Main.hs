{-# Language DeriveDataTypeable, RecordWildCards, TypeSynonymInstances,
    FlexibleInstances #-}
module Main where
import Data.Word
import Foreign.Storable (sizeOf)
import OpenSSL.Random
import Data.Binary.Strict.Get
import System.Random.Mersenne.Pure64 
import Control.Monad 
import Data.List
import Data.BanditSolver.LTS
import Control.Concurrent (getNumCapabilities)
import Control.Parallel.Strategies
import Control.Applicative
import Control.Monad.State
import System.Directory
import System.IO
import Text.Printf
import System.Console.CmdArgs
import System.Exit
import Data.BanditSolver.OBFinder

main :: IO ()
main = do
    opts <- cmdArgs mode
    checkOpts opts
    runMode opts

runMode :: Args -> IO ()
runMode opts@Bandit{..} = do
    print opts
    g <- pureMT `fmap` getOpenSSLRand
    let result = evalState (findOB rounds bestArm badArm armEstimate numArms) g
    mapM_ print result

runMode opts@BruteForce{..}  = do
    print opts
    cores <- getNumCapabilities
    let obNoiseRange = [obStart
                       ,obStart + obStep
                       .. obEnd]
    gens <- map pureMT `fmap` replicateM (length obNoiseRange) getOpenSSLRand
    let results = parMap' cores rseq id . getZipList $ 
            runAveragedLTS bestArm badArm armEstimate numArms rounds repetitions <$> 
                ZipList obNoiseRange <*> ZipList gens
        resultsTr = transpose results -- rows: rounds, columns: ob
    showProgress results
    mapM_ (writeResults opts) resultsTr
  where parMap' n s f = withStrategy (parBuffer n s) . map f

runMode opts@InstantRewards{..} = do
    print opts
    gen <- pureMT `fmap` getOpenSSLRand
    let result = runAveragedInstantRewards bestArm badArm armEstimate numArms
                    rounds repetitions obNoise gen
    mapM_ myPrint result

myPrint (round, reward, dev) = putStrLn $ unwords [show round, show reward, show dev]

writeResults :: Args -> [(Int, Double, Double, Double)] -> IO ()
writeResults BruteForce{..} resultlist = do
    let myRounds = head.fst'.unzip4 $ resultlist
        dir = show numArms ++ "-arms/" ++ show myRounds ++ "-rounds"
        file = dir ++ "/" ++ "good-" ++ show bestArm ++ 
            "_bad-" ++ show (numArms - 1) ++ "-" ++ show badArm ++
            "_estimate-" ++ show armEstimate ++
            "_ob-" ++ showDouble obStart ++ "-" ++ showDouble obEnd ++ 
            "-" ++ showDouble obStep ++ "_reps-" ++ show repetitions ++ ".data"
        fst' (x,_,_,_) = x
        header = "# Rounds: " ++ show myRounds ++ ", repetitions: " ++ show repetitions
            ++ "\n# Good arm ~ N" ++ show bestArm ++ ", " ++ show (numArms - 1)
            ++ " bad arm(s) ~ N" ++ show badArm
            ++ "\n# Observation noise range from " ++ showDouble obStart ++ " to "
            ++ showDouble obEnd ++ " with step size " ++ showDouble obStep
            ++ "\n\n# Observation noise | mean | standard deviation\n\n"
    createDirectoryIfMissing True dir
    writeFile file header
    appendFile file $ unlines (map prettyPrint resultlist)
   
prettyPrint :: (Int, Double, Double, Double) -> String
prettyPrint (_, ob, mean, stddev) = 
    unwords . map showDouble $ [ob, mean, stddev]
    
showRational :: Rational -> String
showRational r =
    let d = fromRational r :: Double
    in printf "%f" d
    
showDouble :: Double -> String
showDouble = printf "%f" 

-- Get a decent random seed
-- randBytes is not thread-safe!
getOpenSSLRand :: IO Word64
getOpenSSLRand = do
    bytes <- randBytes n 
    let (Right w64, _) = runGet getWord64host bytes
    return w64
  where n = sizeOf (undefined :: Word64)


putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ '\r' : s

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
    putProgress $ drawProgressBar 80 0 ++ " " ++ drawPercentage 0
    flip evalStateT 0 $ forM_ xs $ \e -> e `seq` do
        modify (+1)
        progress <- (/len) `fmap` get
        lift $ putProgress $ drawProgressBar 80 progress ++ " " ++ drawPercentage progress
    hPutStrLn stderr " Done." 

data Args = BruteForce
        { obStart :: Double
        , obEnd   :: Double
        , obStep  :: Double
        , rounds  :: Int
        , repetitions :: Int
        , bestArm :: (Double, Double)
        , badArm  :: (Double, Double)
        , armEstimate  :: (Double, Double)
        , numArms  :: Int }
        | Bandit
        { rounds  :: Int
        , bestArm :: (Double, Double)
        , badArm  :: (Double, Double)
        , armEstimate  :: (Double, Double)
        , numArms  :: Int }
        | InstantRewards
        { obNoise :: Double 
        , rounds  :: Int
        , repetitions :: Int
        , bestArm :: (Double, Double)
        , badArm  :: (Double, Double)
        , armEstimate  :: (Double, Double)
        , numArms  :: Int }
        deriving (Data, Typeable, Show, Eq)

bruteforce :: Args
bruteforce = BruteForce
    {rounds      = def
    ,bestArm     = def
    ,badArm      = def
    ,armEstimate = def
    ,numArms     = def
    ,obStart = def &= help "Start for observation noise"
    ,obEnd   = def
    ,obStep  = def
    ,repetitions = def
    } &= help "Use a brute-force method to test all observation noises\
             \ in the specified range."

bandit :: Args
bandit = Bandit
    {rounds      = def
    ,bestArm     = def
    ,badArm      = def
    ,armEstimate = def
    ,numArms     = def
    } &= help "Use an LTS bandit solver to find the best observation noise\
             \ for the specified scenario."

instant :: Args
instant = InstantRewards
    {obNoise     = def
    ,rounds      = def
    ,repetitions = def
    ,bestArm     = def
    ,badArm      = def
    ,armEstimate = def
    ,numArms     = def
    } &= help "Get the instant rewards\
             \ for the specified scenario."


mode :: Args
mode = modes [bandit &= auto, bruteforce, instant] 
        &= help "Find the best observation noise for LTS."
        &= program "Noisy" &= summary "Noisy v0.2"

checkOpts :: Args -> IO ()
checkOpts opts = 
    let requirements = case opts of 
            BruteForce{..} -> 
                [(repetitions < 1, "Repetitions must be > 0.", True)
                ,(obStart <= 0,
                    "Start of observation noise range must be > 0.", True)
                ,(obEnd < obStart,
                    "End of observation noise range must be greater than start.",
                    True)
                ,(obStep <= 0,
                    "Observation noise step must be > 0.", True)
                ]
            Bandit{..} -> []
            InstantRewards{..} ->
                [(obNoise <= 0, "Observation noise must be > 0.", True)
                ,(repetitions < 1, "Repetitions must be > 0.", True)
                ]

            ++  [(rounds opts < 1, "Rounds must be > 0.", True)
                ,(numArms opts < 2, "Number of arms must be > 1.", True)
                ,(armEstimate opts == (0.0,0.0),
                    "Estimate has default value (0.0, 0.0).", False)
                ,(badArm opts == (0.0,0.0),
                    "Bad arm has default value (0.0, 0.0).", False)
                ,(bestArm opts == (0.0,0.0),
                    "Best arm has default value (0.0, 0.0).", False)
                ,((fst $ badArm opts) > (fst $ bestArm opts),
                    "Bad arm must be worse than best arm.", True)
                ]
    in mapM_ handle requirements
  where handle (predicate, msg, fatal) = when predicate $ 
                                            putStrLn (msgType fatal ++ msg) >> when fatal quit
        quit = exitWith (ExitFailure 1)
        msgType fatal = if fatal then "\x1b[1;31mError: \x1b[0m" else "\x1b[0;33mWarning: \x1b[0m"

