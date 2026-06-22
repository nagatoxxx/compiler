module Main where

import Lexer.Rules
import Parser.Rules

import Data.List

main :: IO ()
main = do
    fileContent <- readFile "input.txt"
    putStrLn "[input]"
    putStr fileContent
    case tokenize fileContent of
        Left err     -> putStrLn ("lexer error: " ++ show err)
        Right tokens -> do 
            putStrLn "[lexer]"
            putStrLn (intercalate " " (map show tokens))
            let e = parse tokens
            case e of
                Left err   -> putStrLn ("parser error: " ++ show err)
                Right expr -> putStrLn "[parser]" >> print expr
