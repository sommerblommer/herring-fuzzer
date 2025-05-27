module Main (main) where

import Control.Monad (replicateM, foldM)
import System.Process
import System.Random
import GHC.IO.Exception (ExitCode(ExitFailure))
import qualified Data.Map as M
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Time.Clock
import Text.Printf (printf)
import Plotter (plotStats, plotSaved)
import LoadingBar (loader, createBar)

data Env = Env {funs :: [(String, Int)], recDepth :: Int, localVars :: [String]}

alexPath :: String 
alexPath = "/Users/alexandersommer/Desktop/fritid/haskell/Herring-lang/.stack-work/dist/aarch64-osx/Cabal-3.8.1.0/build/herring-exe/herring-exe"
alexPathToInput :: String 
alexPathToInput = "/Users/alexandersommer/Desktop/Uni/2. semester kand./LBS/herring-fuzzer/fuzzer/input.txt"

samPath :: String 
samPath = "/home/samuel/lbs-projekt/Herring-lang/.stack-work/dist/x86_64-linux/ghc-9.4.8/build/herring-exe/herring-exe"
samPathToInput :: String 
samPathToInput = "/home/samuel/lbs-projekt/herring-fuzzer/fuzzer/input.txt"

type ErrorAcc = M.Map String [(String, String, String, String)] 
type StatAcc = [(Int, Double, String, String)]

parseArgs :: IO [String] -> IO Int
parseArgs x = helper =<< x where

    helper (y:_) =case readMaybe y of 
                    Just i -> return i 
                    Nothing -> error "arg was not an integer"

    helper _ = error "only one argument need"

main :: IO ()
main = do
    amountOfRuns <- parseArgs getArgs
    createBar
    (res, stats) <- loop amountOfRuns amountOfRuns [] M.empty
    writeFile "report.txt" $ formatCategory res
    writeFile "stats.txt" $ collectStats res
    plotStats stats
    plotSaved 

loop :: Int -> Int -> StatAcc -> ErrorAcc -> IO (ErrorAcc, StatAcc)  
loop 0 _ scc acc = return (acc, scc)
loop i total scc acc = do 
    loader i total
    t1 <- getCurrentTime
    input <- mainGadget
    t2 <- getCurrentTime
    writeFile "input.txt" input
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode alexPath [alexPathToInput] ""
    compiled <- readFile "output.ll"  
    t3 <- getCurrentTime
    let delta = t2 `diffUTCTime` t1
    let _delta2 = t3 `diffUTCTime` t1
    foo (length (lines input)) delta exitCode stdErr
    let nscc = (length (lines input), realToFrac delta, show exitCode, stdErr):scc 
    c <- if null $ lines compiled then return "" else  return compiled
    case exitCode of 
        ExitFailure _ ->  loop (i - 1) total nscc $ categorizeError acc input stdOut stdErr c
        _ -> loop (i - 1) total nscc acc


foo :: Int -> NominalDiffTime -> ExitCode -> String -> IO ()
foo len delta exitCode stdErr =
    let cat = categorizer $ words stdErr
    in 

    let str = printf "LoC: %d, time: %.5f seconds, exit status: %s, error: %s\n" 
                len (realToFrac delta :: Double) (show exitCode) cat
    in
    appendFile "runtimes2.txt" str

categorizer :: [String] -> String
categorizer [] = ""
categorizer ("herring-exe:":xs) = categorizer xs 
categorizer (x:_) = x

categorizeError :: ErrorAcc -> String -> String -> String -> String -> ErrorAcc 
categorizeError acc code out err com =  
    let category = categorizer $ words err 
    in case M.lookup category acc of 
       Just c ->  M.insert category ((out,err, code, com):c) acc 
       _ -> M.insert category [(out, err, code, com)] acc

formatCategory :: ErrorAcc -> String
formatCategory acc = 
    let helper x = replicate 100 '*' ++ "\n" ++ replicate 30 '*' ++ x ++ replicate 30 '*' ++ "\n" ++ replicate 100 '*' ++ "\n"
    in M.foldrWithKey (\k v a -> helper k ++ concatMap formatOut v ++ a ) "" acc  

formatOut :: (String,  String, String, String) -> String 
formatOut (stdOut, stdErr, code, com) =
    let stars = replicate 5 '*'
    in let outBanner = stars ++ " stdOut " ++ stars ++ "\n"
    in let codeBanner = stars ++ " code " ++ stars ++ "\n"
    in let errBanner = stars ++ " stdErr " ++ stars ++ "\n"
    in let compBanner = stars ++ " compiled " ++ stars ++ "\n"
    in outBanner ++ stdOut ++ errBanner ++ stdErr ++ codeBanner ++ code ++ compBanner ++ com ++ "\n"

emptyEnv :: Env 
emptyEnv = Env {funs = [], recDepth = 0, localVars = []}

(!?) :: Int -> [a] -> Maybe a 
(!?) a xs 
    | null xs || a >= length xs = Nothing 
    | otherwise = return $ xs !! a

mainGadget :: IO String
mainGadget = do
  let multiFuns e acc = do 
        lottery <- randomRIO (0 :: Int, 100)
        if lottery < 20 then return acc
        else do
            rand <- randomRIO (0 :: Int, 5)
            (fc, nenv) <- functionGadget e rand 
            multiFuns nenv ((fc, nenv) : acc)

  rand <- randomRIO (1 :: Int, 5)
  (fc, en) <- functionGadget emptyEnv rand 
  funs <- multiFuns en [(fc, en)]
  let lenv = snd $ head funs  
  let conc = concatMap (("\n"++) . fst) funs
  exp1 <- expGadget lenv
  (stm, _) <- blockGadget lenv
  return $ conc ++ " main : Int\n" ++ stm ++ "\n\tlet _ = print(" ++ exp1 ++ ")\n\tin return 0\n"


blockGadget :: Env -> IO (String, Env)
blockGadget e = do 
    rn <- randomRIO (0 :: Int, 100)
    if rn <= 40 then return ("", e)
    else do 
        (res, nenv) <- stmGadget e
        (res2, lenv) <- blockGadget nenv 
        return (res ++ "\n" ++ res2, lenv)

stmGadget :: Env -> IO (String, Env)
stmGadget e = do 
    rn <- randomRIO (0 :: Int, 100)
    let decisionTable x 
            | 80 < x && x <= 100 = ifStmGadget e 
            | 70 < x && x  <= 80 = forGadget e 
            | 20 < x && x <= 70 = letBindGadget  e
            | 0 < x && x <= 20 = returnGadget e
            | otherwise = letBindGadget e
    decisionTable rn
letBindGadget :: Env -> IO (String, Env) 
letBindGadget e = do 
    str <- validIdentGadget 
    rhs <- expGadget e  
    let nenv = e {localVars = str : localVars e}
    return ("\tlet " ++ str ++ " = " ++ rhs ++ "\n\tin ", nenv)

returnGadget :: Env -> IO (String, Env)
returnGadget e = expGadget e >>= \expr -> return ("return " ++ expr, e)

ifStmGadget :: Env -> IO (String, Env) 
ifStmGadget e = do 
    cond <- binOpGadget e =<< boolOpGadget
    th <- expGadget e 
    el <- expGadget e 
    let res = "\tif " ++ cond ++ "\nthen " ++ th ++ "\nelse " ++ el
    return (res, e)
    
forGadget :: Env -> IO (String, Env)
forGadget e = do
    num1 <- randomRIO (0 :: Int, 1000)  
    num2 <- randomRIO (num1, 1000)  
    let range = show num1 ++ ".." ++ show num2 
    body <- expGadget e 
    var <- validIdentGadget
    let res = "\tfor " ++ var ++ " in " ++ range ++ " ->\n\t" ++ body ++ "\n\t<-\n" 
    return (res, e)

arrLookupGadget :: Env -> IO String
arrLookupGadget e 
    | null (localVars e) = expGadget e 
    | otherwise = do 
        let lvars = localVars e
        index <- randomRIO (0, length lvars)
        lhs <- maybe validIdentGadget return (index !? lvars)
        lup <- expGadget e
        return $ lhs ++ "[" ++ lup ++ "]"


arrLitGadget :: IO String 
arrLitGadget = do 
    num <- randomRIO (1 :: Int, 5)
    let f 1 = do 
            a <- numGadget 1000 
            return $ a ++ "]"
        f x = do 
            a <- numGadget 1000 
            rest <- f (x - 1) 
            return $ a ++ "," ++ rest
    res <- f num
    return $ "[" ++ res


functionGadget :: Env -> Int -> IO (String, Env) 
functionGadget en argLen = do 
    name <- validIdentGadget 
    let argNames = [validIdentGadget | _ <- [0..argLen]]
    let start = name ++ " : "
    let args :: [IO String] -> IO (String, [String])
        args [] = return (" Int", [])
        args [x] = do 
            arg <- x 
            rtyp1 <- typeGadget
            rtyp2 <- typeGadget
            return ("( " ++ arg ++  " : " ++ rtyp1 ++ ") ->" ++ rtyp2 ++ "\n", [arg])
        args (x:xs) = do 
            arg <- x
            rtyp <- typeGadget
            (rest, restOfArgs) <- args xs
            return ("( " ++ arg ++ " :" ++ rtyp ++ ") -> " ++ rest, arg : restOfArgs)
    (str, genArgs) <- args argNames
    let varenv = foldl (\acc arg -> acc {localVars = arg :localVars acc}) en genArgs 
    (e, le) <- blockGadget varenv
    laste <- expGadget le
    let nenv = en {funs = (name, argLen + 1) : funs en} 
    return (start ++ str ++ e ++ "\treturn " ++ laste ++ "\n", nenv)
    


typeGadget :: IO String 
typeGadget = randomRIO (0 :: Int, 100) >>=  
             \rn -> if rn < 20 then do 
                t <- typeGadget 
                return $ "[" ++ t ++ "]"
             else return "Int"


expGadget :: Env -> IO String
expGadget en = do 
    rn <- randomRIO (0 :: Int, 100)
    let lottery i 
            -- 10%
            | 0 <= i && i < 10 = 
                if recDepth en > 0 || null (funs en) then expGadget en else funcallGadget $ en {recDepth = recDepth en + 1} 
            -- 20%
            | 10 <= i && i < 30 = opGadget >>= binOpGadget en 
            -- 10%
            | 30 <= i && i < 40 =  arrLitGadget 
            -- 10%
            | 40 <= i && i < 50 =   arrLookupGadget en
            -- 20%
            | 50 <= i && i < 70 =   do 
                let len = length $ localVars en
                r <- randomRIO (0 :: Int, len - 1)
                case r !? localVars en of 
                    Just x -> return x 
                    _ -> expGadget en
            -- 30% 
            | otherwise =  numGadget 1000
    lottery rn

binOpGadget :: Env -> String -> IO String 
binOpGadget en op = do 
    left <- expGadget en 
    right <- expGadget en 
    return $ left ++ op ++ right

funcallGadget :: Env -> IO String 
funcallGadget en = do
        let len = length $ funs en 
        num <- randomRIO (0, len - 1) 
        let (fname, argLen) = funs en !! num
        let args :: Int -> IO String 
            args 1 = expGadget en 
            args n = do 
                e <- expGadget en 
                rest <- args (n - 1)  
                return $ e ++ ", " ++ rest
        a <- args argLen 
        return $ "(" ++ fname ++ "(" ++ a ++ "))"

   
boolOpGadget :: IO String 
boolOpGadget = do
    a <- randomRIO (1 :: Int, 5 :: Int)
    case a of 
        1 -> return "<="
        2 -> return "<"
        3 -> return ">"
        4 -> return ">="
        _ -> return "=="
    

numGadget :: Int ->  IO String
numGadget i = show <$> randomRIO (0, i) 

opGadget :: IO String 
opGadget = do
    a <- randomRIO (1 :: Int, 4 :: Int)
    case a of 
        1 -> return "+"
        2 -> return "-"
        3 -> return "*"
        4 -> return "/"
        _ -> return "+"

strGadget :: IO String
strGadget = flip replicateM (randomRIO (' ', '~')) =<< randomRIO (1, 32)


validIdentGadget :: IO String 
validIdentGadget  = flip replicateM (randomRIO ('a', 'z')) =<< randomRIO (1, 4)


collectStats :: ErrorAcc -> String
collectStats = M.foldrWithKey (\k v acc -> "category: " ++  k ++ ", errors: " ++ show (length v) ++ "\n" ++ acc) "" 
