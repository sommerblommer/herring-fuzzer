module Main (main) where

import Control.Monad (replicateM, foldM)
import System.Process
import System.Random
import GHC.IO.Exception (ExitCode(ExitFailure))
import qualified Data.Map as M


data Env = Env {funs :: [(String, Int)], recDepth :: Int, localVars :: [String]}

alexPath :: String 
alexPath = "/Users/alexandersommer/Desktop/fritid/haskell/Herring-lang/.stack-work/dist/aarch64-osx/Cabal-3.8.1.0/build/herring-exe/herring-exe"
alexPathToInput :: String 
alexPathToInput = "/Users/alexandersommer/Desktop/Uni/2. semester kand./LBS/herring-fuzzer/fuzzer/input.txt"

samPath :: String 
samPath = "/home/samuel/lbs-projekt/Herring-lang/.stack-work/dist/x86_64-linux/ghc-9.4.8/build/herring-exe/herring-exe"
samPathToInput :: String 
samPathToInput = "/home/samuel/lbs-projekt/herring-fuzzer/fuzzer/input.txt"

type ErrorAcc = M.Map String [(String, String, String)]

main :: IO ()
main = do 
    writeFile "report.txt" ""
    loop 100 M.empty
loop :: Int -> ErrorAcc -> IO ()  
loop 0 acc = 
    writeFile "report.txt" $ formatCategory acc
loop i acc = do 
  putStrLn $ "genereating No. " ++ show i
  input <- mainGadget
  writeFile "input.txt" input

  (exitCode, stdOut, stdErr) <- readProcessWithExitCode alexPath [alexPathToInput] ""
  case exitCode of 
    ExitFailure _ ->  loop (i - 1) $ categorizeError acc input stdOut stdErr 
    _ -> loop (i - 1) acc

categorizeError :: ErrorAcc -> String -> String -> String -> ErrorAcc 
categorizeError acc code out err =  
    let helper [] = ""
        helper ("herring-exe:":xs) = helper xs 
        helper (x:_) = x
    in let category = helper $ words err 
    in case M.lookup category acc of 
       Just c ->  M.insert category ((out,err, code):c) acc 
       _ -> M.insert category [(out, err, code)] acc

formatCategory :: ErrorAcc -> String
formatCategory acc = 
    let helper x = replicate 100 '*' ++ "\n" ++ replicate 30 '*' ++ x ++ replicate 30 '*' ++ "\n" ++ replicate 100 '*' ++ "\n"
    in M.foldrWithKey (\k v a -> helper k ++ concatMap formatOut v ++ a ) "" acc  

formatOut :: (String,  String, String) -> String 
formatOut (code, stdOut, stdErr) =
    let stars = replicate 5 '*'
    in let outBanner = stars ++ " stdOut " ++ stars ++ "\n"
    in let codeBanner = stars ++ " code " ++ stars ++ "\n"
    in let errBanner = stars ++ " stdErr " ++ stars ++ "\n"
    in outBanner ++ stdOut ++ errBanner ++ stdErr ++ codeBanner ++ code ++ "\n"

emptyEnv :: Env 
emptyEnv = Env {funs = [], recDepth = 0, localVars = []}

(!?) :: Int -> [a] -> Maybe a 
(!?) a xs 
    | length xs < a = Nothing 
    | otherwise = return $ xs !! a

mainGadget :: IO String
mainGadget = do
  rand <- randomRIO (0 :: Int, 5)
  (fc, en) <- functionGadget emptyEnv rand 
  exp1 <- expGadget en
  (stm, _) <- stmGadget en
  return $ fc ++ " main : Int\n" ++ stm ++ "\n\tlet _ = print(" ++ exp1 ++ ")\n\tin return 0\n"


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
    return ("let " ++ str ++ " = " ++ rhs, nenv)

returnGadget :: Env -> IO (String, Env)
returnGadget e = expGadget e >>= \expr -> return ("return " ++ expr, e)

ifStmGadget :: Env -> IO (String, Env) 
ifStmGadget e = do 
    cond <- binOpGadget e
    th <- expGadget e 
    el <- expGadget e 
    let res = "if " ++ cond ++ "\nthen " ++ th ++ "\nelse " ++ el
    return (res, e)
    
forGadget :: Env -> IO (String, Env)
forGadget e = do
    num1 <- numGadget 1000  
    num2 <- numGadget 1000  
    let range = num1 ++ ".." ++ num2 
    body <- expGadget e 
    var <- validIdentGadget
    let res = "for " ++ var ++ " in " ++ range ++ " ->\n" ++ body ++ "\n<-\n" 
    return (res, e)

arrLookupGadget :: Env -> IO String
arrLookupGadget e = do 
    let lvars = localVars e
    index <- randomRIO (0, length lvars)
    lhs <- case index !? lvars of 
              Just x -> return x  
              Nothing -> validIdentGadget
    lup <- expGadget e
    return $ lhs ++ "[" ++ lup ++ "]"


arrLitGadget :: IO String 
arrLitGadget = do 
    num <- randomRIO (1 :: Int, 5)
    let f 0 = return "]"
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
            return ("( " ++ arg ++  " : Int) -> Int\n", [arg])
        args (x:xs) = do 
            arg <- x
            (rest, restOfArgs) <- args xs
            return ("( " ++ arg ++ " : Int) -> " ++ rest, arg : restOfArgs)
    (str, genArgs) <- args argNames
    let varenv = foldl (\acc arg -> acc {localVars = arg :localVars acc}) en genArgs 
    e <- expGadget varenv
    let nenv = en {funs = (name, argLen + 1) : funs en} 
    return (start ++ str ++ "\treturn " ++ e ++ "\n", nenv)
    



expGadget :: Env -> IO String
expGadget en = do 
    lottery <- randomRIO (0 :: Int, 4)
    case lottery of 
        1 -> if recDepth en > 0 || null (funs en) then expGadget en else funcallGadget $ en {recDepth = recDepth en + 1} 
        2 -> binOpGadget en
        3 -> do 
            let len = length $ localVars en
            case len of 
                0 -> expGadget en
                _ -> do
                    r <- randomRIO (0 :: Int, len - 1)
                    return $ localVars en !! r
        _ -> numGadget 1000

binOpGadget :: Env -> IO String 
binOpGadget en = do 
    left <- expGadget en 
    op <- opGadget 
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
