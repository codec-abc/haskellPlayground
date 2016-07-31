--solve challenge 270
--https://www.reddit.com/r/dailyprogrammer/comments/4msu2x/challenge_270_easy_transpose_the_input_text/

module TransposeLines (transpose) where

longestLine :: [String] -> Int
longestLine = foldr (max . length) 0

normalizeLines :: [String] -> [String]
normalizeLines a = 
    map (\aLine -> aLine ++ replicate (longestLine a - (length aLine)) ' ') a
    
transposeUnsafe :: [String] -> [String]
transposeUnsafe a = foldr mappage accum a where
    accum = replicate (length (a !! 0)) ""
    mappage = zipWith (\str strList -> [str] ++ strList)

transpose :: [String] -> [String]
transpose = transposeUnsafe . normalizeLines