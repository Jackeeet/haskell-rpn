import           Control.Exception.Base
import           Control.Monad.State
import           Data.Map.Strict        (fromAscList, fromList, (!))
import           System.IO

data Token = NumToken Int | OpToken (Int -> Int -> Int)
type ValueStack = [Int]

opTokens = ["+", "-", "/", "*"]

ops = fromList $ zip opTokens [(+), (-), div, (*)]

main = do
    putStr "> "
    hFlush stdout
    line <- getLine
    if null line
        then return ()
        else evalInput line `catch` mathHandler

evalInput line = do
    let ws = words line
    let tokens = parse ws
    let result =  head $ execState (mapM readToken tokens) []   -- mapM == sequence $ map
    putStrLn $ "result: " ++ show result
    main

parse :: [String] -> [Token]
parse = map (\x -> if x `elem` opTokens
    then OpToken $ ops ! x
    else NumToken $ read x)

readToken :: Token -> State ValueStack ()
readToken t = case t of
    NumToken x  -> do
        xs <- get
        put (x:xs)
    OpToken op  -> do
        xs <- get
        let b = head xs
        let a = head . tail $ xs
        put $ op a b:drop 2 xs

mathHandler :: ArithException -> IO ()
mathHandler DivideByZero = putStrLn "err: division by 0"
mathHandler _            = putStrLn "err: could not evaluate"
