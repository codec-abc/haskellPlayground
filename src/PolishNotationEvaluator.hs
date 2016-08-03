module PolishNotationEvaluator (evaluate) where

import Data.Maybe (isNothing, fromJust)
import Data.Either (lefts, rights, isRight)
import Text.Read (readMaybe)

--https://en.wikipedia.org/wiki/PolishNotation_notation

data PolishNotationSymbol = PolishNotationOperator ((Double -> Double -> Double), String) | PolishNotationOperand (Double)
type PolishNotationParseResult = Either PolishNotationSymbol String

instance Show PolishNotationSymbol where
    show w = case w of
        PolishNotationOperator (x,name) -> name
        PolishNotationOperand operand -> (show operand)

type Stack = [Double]
type PolishNotationEvaluationState = ([PolishNotationSymbol], Stack)
type PolishNotationStepResult = Either PolishNotationEvaluationState String

-- todo : handle empty stack, empty operators with multiple numbers
step :: PolishNotationEvaluationState -> PolishNotationStepResult
step (firstSymbol:otherSymbols, stack) =
    case firstSymbol of
        PolishNotationOperator (operator, name) -> let
            hasEnoughstack = (length stack) >= 2 in
            if hasEnoughstack then let
                first = stack !! 0
                second = stack !! 1
                result = operator first second in
                Left $ (otherSymbols, [result] ++ (drop 2 stack))
            else
                Right $ "There is not enough stack when trying to apply function " ++ name ++ " ."
        PolishNotationOperand operand -> Left (otherSymbols, ([operand] ++ stack))

step ([] , stack) =
    let nb = length stack in
        if nb == 1 then
            Left ([] , stack)
        else
            Right $ "There is no more operator or operand and the stack contains " ++ (show nb) ++ " elements."

isDone :: PolishNotationEvaluationState -> Bool
isDone (polishSymbols, stack) = 
    ((length polishSymbols) == 0) && ((length stack) == 1)

run2 :: PolishNotationEvaluationState -> Int -> Either Double String
run2 a b = 
    let iter = step a in
        case iter of
            Left (polishSymbols, stack) -> 
                if isDone (polishSymbols, stack) then
                    Left $ head stack
                else
                    run2 (polishSymbols, stack) $ b + 1
            Right error -> Right (error ++ " At iteration " ++ (show b))

run :: PolishNotationEvaluationState -> Either Double String     
run a = run2 a 0

convertStringToPolishNotationSymbol :: String -> PolishNotationParseResult
convertStringToPolishNotationSymbol aString =
    case aString of
        "+"  -> Left $ PolishNotationOperator ((+), "(+)")
        "-"  -> Left $ PolishNotationOperator ((-), "(-)")
        "*"  -> Left $ PolishNotationOperator ((*), "(*)")
        "/"  -> Left $ PolishNotationOperator ((/), "(/)")
        aStr -> let maybeSymbol = (fmap) PolishNotationOperand ((readMaybe aStr)::Maybe Double) in
            case maybeSymbol of
                Just x -> Left $ x
                Nothing -> Right $ "Cannot parse token " ++ aStr

parseInput :: String -> [PolishNotationParseResult]
parseInput aString = reverse $ map convertStringToPolishNotationSymbol $ words aString

evaluate :: String -> Either Double String
evaluate inputString = let
    parseResult = parseInput inputString
    isOk = not (any isRight parseResult) in
    if isOk then
        let filteredResult = lefts parseResult in
            run (filteredResult, [])
    else
        Right $ head $ rights parseResult
