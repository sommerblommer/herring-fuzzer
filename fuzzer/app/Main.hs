module Main (main) where

import Control.Monad (replicateM, foldM)
import System.Process
import System.Random
import System.Random.Stateful (Uniform (uniformM), StatefulGen (uniformWord8))
import Data.Bits


data Env = Env {funs :: [(String, Int)]}


main :: IO ()
main = do
  input <- mainGadget
  writeFile "input.txt" input

  let alex_path = "/Users/alexandersommer/Desktop/fritid/haskell/Herring-lang/.stack-work/dist/aarch64-osx/Cabal-3.8.1.0/build/herring-exe/herring-exe"
  let alex_pathToInput = "/Users/alexandersommer/Desktop/Uni/2. semester kand./LBS/herring-fuzzer/fuzzer/input.txt"

  let sam_path = "/home/samuel/lbs-projekt/Herring-lang/.stack-work/dist/x86_64-linux/ghc-9.4.8/build/herring-exe/herring-exe"
  let sam_pathToInput = "/home/samuel/lbs-projekt/herring-fuzzer/fuzzer/input.txt"

  (exitCode, stdOut, stdErr) <- readProcessWithExitCode alex_path [alex_pathToInput] ""
  putStrLn $
    "Exit code: "
      ++ show exitCode
      ++ "\nOut: "
      ++ stdOut
      ++ "\nErr: "
      ++ stdErr


emptyEnv :: Env 
emptyEnv = Env {funs = []}

mainGadget :: IO String
mainGadget = do
  (fc, en) <- functionGadget emptyEnv 10 
  exp1 <- expGadget en
  rStr <- strGadget
  exp2 <- expGadget en
  funcall <- funcallGadget en
  op <- opGadget
  return $ fc ++ " main : Int\n\tlet _ = print(" ++ exp1 ++ op ++ funcall ++ ")\n\tin return 0\n"



functionGadget :: Env -> Int -> IO (String, Env) 
functionGadget en argLen = do 
    name <- validIdentGadget 
    let argNames = [validIdentGadget | _ <- [0..argLen]]
    let start = name ++ " : "
    let args :: [IO String] -> IO String
        args [] = return " Int"
        args [x] = do 
            arg <- x
            return $ "( " ++ arg ++  " : Int) -> Int\n" 
        args (x:xs) = do 
            arg <- x
            rest <- args xs
            return $ "( " ++ arg ++ " : Int) -> " ++ rest
    str <- args argNames
    let nenv = en {funs = (name, argLen) : funs en} 
    e <- expGadget nenv
    return (start ++ str ++ "\treturn " ++ e ++ "\n", nenv)
    



expGadget :: Env -> IO String
expGadget en = do 
    lottery <- randomRIO (0 :: Int, 5)
    putStrLn $ "first Lottery : " ++ show lottery
    case lottery of 
        _ -> numGadget 99999999

funcallGadget :: Env -> IO String 
funcallGadget en 
    | null (funs en) = return "" 
    | otherwise = do 
        let len = length $ funs en 
        putStrLn $ "amount of funs : " ++ show len
        num <- randomRIO (0, len - 1) 
        putStrLn $ "lottery : " ++ show  num
        let (fname, argLen) = funs en !! num
        let args :: Int -> IO String 
            args 1 = expGadget en 
            args n = do 
                e <- expGadget en 
                rest <- args (n - 1)  
                return $ e ++ ", " ++ rest
        a <- args (argLen + 1) 
        return $ "(" ++ fname ++ "(" ++ a ++ "))"

    
    

numGadget :: Int ->  IO String
numGadget i = do
  r <- randomRIO (0, i)
  return $ show r

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
strGadget = do
  flip replicateM (randomRIO (' ', '~')) =<< randomRIO (1, 32)

validIdentGadget :: IO String 
validIdentGadget  = flip replicateM (randomRIO ('a', 'z')) =<< randomRIO (1, 32)
