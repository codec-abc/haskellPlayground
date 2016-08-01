module Challenge263 (entropy) where

import Data.List

log2 :: Double -> Double
log2 x = (log x / log 2)

entropy2 :: String -> [(Char, Int)]
entropy2 x = 
    let 
        withoutDups = nub x
        count aChar = length . filter (\aChar2 -> aChar == aChar2)
    in    
        map (\aChar -> (aChar, count aChar x)) withoutDups
        
entropy3 :: [(Char, Int)] -> Double
entropy3 input = 
        let
            total = fromIntegral (sum $ map snd input)
            mappage (char, occur) accum = accum + occur2/total * log2 (occur2/total)
                    where occur2 = fromIntegral occur
        in
            -1 * (foldr mappage 0.0 input) 

entropy = entropy3 . entropy2