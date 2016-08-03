module PolishEvaluator (evaluate, someString, someString2) where

import Data.Maybe (isNothing, fromJust)
import Data.Either (lefts, rights, isRight)
import Text.Read (readMaybe)

--https://en.wikipedia.org/wiki/Polish_notation

someString = "- 5 * 6 7"
someString2 = "- * / 15 - 7 + 1 1 3 + 2 + 1 1"

data PolishSymbol = PolishOperator ((Double -> Double -> Double), String) | PolishOperand (Double)
type PolishParseResult = Either PolishSymbol String

instance Show PolishSymbol where
    show w = case w of
        PolishOperator (x,name) -> name
        PolishOperand operand -> (show operand)


-- todo : handle empty stack, empty operators with multiple numbers
reduce :: [PolishSymbol] -> [Double] -> Either Double String

reduce (firstSymbol:otherSymbols) numbers =
    case firstSymbol of
        PolishOperator (operator, name) -> let
            hasEnoughNumbers = (length numbers) >= 2 in
            if hasEnoughNumbers then let
                first = numbers !! 0
                second = numbers !! 1
                result = operator first second in
                reduce otherSymbols $ [result] ++ (drop 2 numbers)
            else
                Right $ "There is not enough numbers when trying to apply function " ++ name
        PolishOperand operand -> reduce otherSymbols ([operand] ++ numbers)

reduce [] numbers =
    let nb = length numbers in
        if nb == 1 then
            Left $ head numbers
        else
            Right $ "There is no more operator and operand and the stack contains " ++ (show nb) ++ " elements."

convertStringToPolishSymbol :: String -> PolishParseResult
convertStringToPolishSymbol aString =
    case aString of
        "+"  -> Left $ PolishOperator ((+), "+")
        "-"  -> Left $ PolishOperator ((-), "-")
        "*"  -> Left $ PolishOperator ((*), "*")
        "/"  -> Left $ PolishOperator ((/), "/")
        aStr -> let maybeSymbol = (fmap) PolishOperand ((readMaybe aStr)::Maybe Double) in
            case maybeSymbol of
                Just x -> Left $ x
                Nothing -> Right $ "Cannot parse token " ++ aStr

parseInput :: String -> [PolishParseResult]
parseInput aString = reverse $ map convertStringToPolishSymbol $ words aString

evaluate :: String -> Either Double String
evaluate inputString = let
    parseResult = parseInput inputString
    isOk = not (any isRight parseResult) in
    if isOk then
        let filteredResult = lefts parseResult in
            reduce filteredResult  []
    else
        Right $ head $ rights parseResult
