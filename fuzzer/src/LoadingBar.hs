module LoadingBar where 
import System.Console.ANSI
import System.IO (hFlush, stdout)
import GHC.Float (int2Double, double2Int)

createBar :: IO () 
createBar = do 
    saveCursor 
    putStr "\9474"
    hFlush stdout 
    cursorForward 103
    putStr "\127958"
    hFlush stdout 
    restoreCursor



loader :: Int -> Int -> IO ()
loader current max = do
    let normalize = double2Int $ int2Double ((max - current) * 100) / int2Double max 
    saveCursor 
    cursorForward $ 1 + normalize 
    putStr "\9608\127940\127754"
    hideCursor
    restoreCursor
    hFlush stdout

restore :: IO () 
restore = showCursor
