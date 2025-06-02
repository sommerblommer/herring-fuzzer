module Plotter (plotStats, plotFromData) where 
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Map as M 
import GHC.Float (int2Double)


type Stats =  [(Int, Double, String, String)] 

plotStats :: [(Int, Double, String, String)] -> IO () 
plotStats stats = 
    let avgs = calcAvg stats
    in toFile def "stats.png" $ do
        layout_title .= "Average runtime/LoC"
        layout_x_axis . laxis_override .= axisGridHide
        layout_y_axis . laxis_override .= axisGridHide
        plot (line "a" [[(d,v) | (d,v) <- avgs]])

plotAmount :: Stats -> IO () 
plotAmount stats = 
    let avgs = sumLoCs stats
    in toFile def "amounts.png" $ do
        layout_title .= "Generated Programs / LoC"
        layout_x_axis . laxis_override .= axisGridHide
        layout_y_axis . laxis_override .= axisGridHide
        layout_y_axis . laxis_title .= "# generated programs"
        layout_x_axis . laxis_title .= "lines of code"

        plot (line "generated programs" [[(d,v) | (d,v) <- avgs]])


sumLoCs :: Stats -> [(Int, Int)]
sumLoCs = M.toList . foldr (\(loc, _, _, _) -> M.insertWith (+) loc 1) M.empty 

plotFromData :: IO () 
plotFromData = do 
    stats <- readSavedStats
    plotAmount stats 
    plotSaved stats

plotSaved :: Stats ->  IO () 
plotSaved stats = 
    let avgs = calcAvg stats
    in toFile def "saved.png" $ do 
        layout_title .= "Average Time To Generate Per LoC"
        layout_y_axis . laxis_title .= "time in seconds"
        layout_x_axis . laxis_title .= "lines of code"
        plot (line "" [[(d,v) | (d,v) <- avgs]])

readSavedStats :: IO Stats 
readSavedStats =  fmap (parseSavedStats .  split ',') . filter (/="") . lines <$> readFile "runtimes2.txt"

parseSavedStats :: [String] -> (Int, Double, String, String)
parseSavedStats [loc,time,es,err] =
    let l = read . last . split ' '. last $ split ':' loc
        t = read . (\(_:x:_) -> x) . split ' ' . last $ split ':' time
        e = last $ split ':' es
        errm = last $ split ':' err
    in
    (l,t,e,errm)
parseSavedStats a = error $ "could not parse saved stats " ++ show a  

split :: Char -> String -> [String]
split b s =  helper b s [] where 
    helper :: Char -> String -> String -> [String]
    helper _ [] acc = [reverse acc]
    helper c (x:xs) acc 
        | c == x = reverse acc : helper c xs [] 
        | otherwise =  helper c xs (x : acc)

calcAvg :: [(Int, Double, String, String)] -> [(Int, Double)]
calcAvg stats =  
    let m = foldr (\(loc, time, _, _) acc ->
                    M.insertWith (++) loc [time] acc
                  ) M.empty stats 
    in M.foldMapWithKey (\loc times acc -> 
                            (loc, sum times / int2Double (length times)) : acc
                        ) m []
