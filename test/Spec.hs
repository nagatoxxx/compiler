module Main where

import Test.Hspec

import LexerSpec        qualified as Lexer
import ParserSpec       qualified as Parser
import IntermediateSpec qualified as Intermediate

main :: IO ()
main = hspec $ do
    describe "Lexer"        Lexer.spec
    describe "Parser"       Parser.spec
    describe "Intermediate" Intermediate.spec
