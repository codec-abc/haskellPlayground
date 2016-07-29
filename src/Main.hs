module Main where
    import Triangle
    import PascalTriangle
    import Data.List
    
    iter = PascalTriangle.getIter
    baseTri = PascalTriangle.basePascalTriangle
    
    main = let
        z = iter 5 baseTri in
            do
                putStrLn $ show z
                putStrLn $ show Triangle.firstTri
                putStrLn $ show Triangle.secondTri
                putStrLn $ show Triangle.thirdTri
                

