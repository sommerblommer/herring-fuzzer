module Main (main) where

import System.Process 
import System.Random



main :: IO () 
main = do
    input <- mainGadget
    writeFile "input.txt" input
    let path = "/Users/alexandersommer/Desktop/fritid/haskell/Herring-lang/.stack-work/dist/aarch64-osx/Cabal-3.8.1.0/build/herring-exe/herring-exe"
    let pathToInput = "/Users/alexandersommer/Desktop/Uni/2. semester kand./LBS/herring-fuzzer/fuzzer/input.txt"
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode path [pathToInput] ""
    putStrLn
        $  "Exit code: "
        ++ show exitCode
        ++ "\nOut: "
        ++ stdOut
        ++ "\nErr: "
        ++ stdErr



mainGadget :: IO String 
mainGadget = do 
    exp1 <- expGadget 
    return $ "main : Int let _ = print(" ++ exp1 ++ ") in return 0\n"



expGadget :: IO String 
expGadget = numGadget

numGadget :: IO String 
numGadget = do
    r <- getStdRandom (randomR (0 :: Int, 100000 :: Int))
    return $ show r
