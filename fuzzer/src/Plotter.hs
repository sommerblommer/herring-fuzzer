module Plotter (plotStats, plotSaved) where 
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Map as M 
import GHC.Float (int2Double)


type Stats =  [(Int, Double, String, String)] 

plotStats :: [(Int, Double, String, String)] -> IO () 
plotStats stats = 
    let avgs = calcAvg stats
    in toFile def "stats.png" $ do
        layout_title .= "average runtime/LoC"
        layout_x_axis . laxis_override .= axisGridHide
        layout_y_axis . laxis_override .= axisGridHide
        plot (line "a" [[(d,v) | (d,v) <- avgs]])


plotSaved :: IO () 
plotSaved = 
    readSavedStats >>= 
    \stats ->   let avgs = calcAvg stats
                in toFile def "saved.png" $ do 
                layout_title .= "average runtime/LoC"
                layout_x_axis . laxis_override .= axisGridHide
                layout_y_axis . laxis_override .= axisGridHide
                plot (line "saved" [[(d,v) | (d,v) <- avgs]])

readSavedStats :: IO Stats 
readSavedStats =  fmap (parseSavedStats .  split ',') . filter (/="") . lines <$> readFile "runtimes2.txt"
--    a <- filter (/="") . lines <$> readFile "runtimes2.txt"
--    print $ fmap (map (split ':') . split ',') a
--    return []

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
