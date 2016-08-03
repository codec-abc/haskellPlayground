module Main where

import Triangle
import PascalTriangle
import Data.List
import TransposeLines
import Challenge263
import PolishNotationEvaluator

someString = "- 5 * 6 7"
someString2 = "- * / 15 - 7 + 1 1 3 + 2 + 1 1"
someString3 = "* / 15 - 7 + 1 1 3 + 2 + 1 1"
someString4 = "* / 15 - 7 + 1 1 3 + 2 + 1"
someString5 = "* 15 - 7 + 1 1 3 + 2 + 1 1"

main = do
    putStrLn ( show ( evaluate someString))
    putStrLn ( show ( evaluate someString2))
    putStrLn ( show ( evaluate someString3))
    putStrLn ( show ( evaluate someString4))
    putStrLn ( show ( evaluate someString5))


