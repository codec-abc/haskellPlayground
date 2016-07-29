module Triangle ( 
                  firstTri
                , secondTri
                , thirdTri
                ) where
                
    import Data.List
    
    data Triangle = Triangle 
        {       firstSide :: Int
              , secondSide :: Int
              , thirdSide :: Int
        }
        
    data Triangle2 = Triangle2 Int Int Int
        
    triangleBuild :: Int -> Int -> Int -> Maybe Triangle    
    triangleBuild a b c =
        let areValuesValid = a > 0 && b > 0 && c > 0 in
        if areValuesValid then
         Just Triangle {
              firstSide = a
             ,secondSide = b
             ,thirdSide = c
         }
         else
            Nothing
    
    isTriangleRectangle :: Triangle -> Bool
    isTriangleRectangle tri =
     let at = firstSide tri
         bt = secondSide tri
         ct = thirdSide tri
         [a,b,c] = Data.List.sort [at,bt,ct] in
            a * a + b * b == c * c
        
    firstTri = fmap isTriangleRectangle $ triangleBuild 8 10 6
    secondTri = fmap isTriangleRectangle $ triangleBuild 16 20 12
    thirdTri = fmap isTriangleRectangle $ triangleBuild 6 4 (-1)