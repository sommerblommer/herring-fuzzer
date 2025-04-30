module Main (main) where

import Control.Monad (replicateM)
import System.Process
import System.Random
import System.Random.Stateful (Uniform (uniformM))
import Text.Read (Lexeme (String))

main :: IO ()
main = do
  input <- mainGadget
  writeFile "input.txt" input

  let alex_path = "/Users/alexandersommer/Desktop/fritid/haskell/Herring-lang/.stack-work/dist/aarch64-osx/Cabal-3.8.1.0/build/herring-exe/herring-exe"
  let alex_pathToInput = "/Users/alexandersommer/Desktop/Uni/2. semester kand./LBS/herring-fuzzer/fuzzer/input.txt"

  let sam_path = "/home/samuel/lbs-projekt/Herring-lang/.stack-work/dist/x86_64-linux/ghc-9.4.8/build/herring-exe/herring-exe"
  let sam_pathToInput = "/home/samuel/lbs-projekt/herring-fuzzer/fuzzer/input.txt"

  (exitCode, stdOut, stdErr) <- readProcessWithExitCode sam_path [sam_pathToInput] ""
  putStrLn $
    "Exit code: "
      ++ show exitCode
      ++ "\nOut: "
      ++ stdOut
      ++ "\nErr: "
      ++ stdErr

mainGadget :: IO String
mainGadget = do
  exp1 <- expGadget
  rStr <- strGadget
  return $ "main : Int let " ++ rStr ++ " = print(" ++ exp1 ++ ") in return 0\n"

expGadget :: IO String
expGadget = numGadget

numGadget :: IO String
numGadget = do
  r <- getStdRandom (randomR (0 :: Int, 100000 :: Int))
  return $ show r

strGadget :: IO String
strGadget = do
  flip replicateM (randomRIO (' ', '~')) =<< randomRIO (1, 32)
