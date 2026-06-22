module Main where

import Test.Hspec

import Lexer.Rules
import Lexer.Token
import LexerSpec

main :: IO ()
main = hspec $ spec
