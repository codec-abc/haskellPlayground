module PascalTriangle where
    import Data.List
    data PascalTriangle = PascalTriangle [Int] deriving (Show)
    
    basePascalTriangle = PascalTriangle[1]
    
    generateNext (x, a:[]) = (x ++ [a], [])
    generateNext (x, a:b:y) = generateNext (x ++ [(a + b)], (b:y)) 
    
    nextTriangle (PascalTriangle a) = PascalTriangle newList where
        newList = fst $ generateNext ([], 0:a)
        
    getIter n tri = let w = Data.List.iterate nextTriangle tri in 
        head (drop n w)