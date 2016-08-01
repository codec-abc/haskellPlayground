module Main where
    import Triangle
    import PascalTriangle
    import Data.List
    import TransposeLines
    import Challenge263
    
    iter = PascalTriangle.getIter
    baseTri = PascalTriangle.basePascalTriangle
    transposed = TransposeLines.transpose ["Some","Text."]    
    main = let
        z = iter 5 baseTri in
            do
                -- putStrLn $ show transposed
                -- putStrLn $ show z
                -- putStrLn $ show Triangle.firstTri
                -- putStrLn $ show Triangle.secondTri
                -- putStrLn $ show Triangle.thirdTri
                putStr ""

