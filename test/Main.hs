module Main (main) where

import Data.List (sort)

import Test.Tasty
import Test.Tasty.HUnit

import Lexer.Automata.DFA
import Lexer.Automata.NFA
import Lexer.Automata.NFALambda

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [
                            dfaUnitTests
                          , nfaUnitTests
                          , subsetTests
                          , lambdaClosureTests
                          , nfaLamTests
                          ]

dfaUnitTests :: TestTree
dfaUnitTests = testGroup "Tests for DFA"
                        [
                          testCase "Accepts empty string" $ dfaAccept dfaSample "" @?= True
                        , testCase "Accepts strings with only ones" $ dfaAccept dfaSample "1111" @?= True
                        , testCase "Accepts strings with even zeros" $ dfaAccept dfaSample "100111100" @?= True
                        , testCase "Rejects strings with odd zeros" $ dfaAccept dfaSample "0111" @?= False
                        ]


nfaUnitTests :: TestTree
nfaUnitTests = testGroup "Tests for NFA"
                         [
                           testCase "Rejects the empty string" $ nfaAccept nfaSample "" @?= False
                         , testCase "Reject 10" $ nfaAccept nfaSample "10" @?= False
                         , testCase "Accepts 00" $ nfaAccept nfaSample "00" @?= True
                         , testCase "Accepts 1110000" $ nfaAccept nfaSample "1110000" @?= True
                         ]


subsetTests :: TestTree
subsetTests = testGroup "Tests for subset construction"
                         [
                           testCase "Rejects the empty string" $ dfaAccept m "" @?= False
                         , testCase "Reject 10" $ dfaAccept m "10" @?= False
                         , testCase "Accepts 00" $ dfaAccept m "00" @?= True
                         , testCase "Accepts 1110000" $ dfaAccept m "1110000" @?= True
                         ]
        where
          m = subset nfaSample


lambdaClosureTests :: TestTree
lambdaClosureTests
  = testGroup "Tests for lambda closure"
              [
                testCase "Closure for I" $ (sort $ lambdaClosure nfaLamSample ["I"]) @?= (sort ["I", "ZP"])
              , testCase "Closure for UI" $ lambdaClosure nfaLamSample ["UI"] @?= ["UI"]
              ]


nfaLamTests :: TestTree
nfaLamTests
  = testGroup "Tests for NFA lambda"
              [
                testCase "Accepts the empty string" $ nfaLamAccept nfaLamSample "" @?= True
              , testCase "Accepts even number of 0s" $ nfaLamAccept nfaLamSample "00" @?= True
              , testCase "Rejects odd number of 0s" $ nfaLamAccept nfaLamSample "0" @?= False
              , testCase "Accepts odd number of 1s" $ nfaLamAccept nfaLamSample "1" @?= True
              , testCase "Rejects even number of 1s" $ nfaLamAccept nfaLamSample "11" @?= False
              ]


-- sample DFA for testing: accepts an even number of 0's

dfaSample :: DFA String Char
dfaSample
  = DFA ["P", "I"]
        "01"
        [
          (("P", '1'), "P")
        , (("P",'0'), "I")
        , (("I", '1'), "I")
        , (("I",'0'),"P")
        ]
        "P"
        ["P"]


-- sample NFA for testing: accepts words ending with 00

nfaSample :: NFA String Char
nfaSample
  = NFA ["Z", "O", "T"]
        "01"
        [
          (("Z",'0'), ["Z", "O"])
        , (("Z", '1'), ["Z"])
        , (("O", '0'), ["T"])
        , (("O",'1'), [])
        , (("T",'0'),[])
        , (("T",'1'),[])
        ]
        ["Z"]
        ["T"]

-- sample NFA lambda for testing

nfaLamSample :: NFALam String Char
nfaLamSample
  = NFALam ["I", "ZP", "ZI", "UP", "UI"]
           "01"
           [
             (("I", Just '1'), ["UI"])
           , (("I", Nothing), ["ZP"])
           , (("I", Just '0'), [])
           , (("ZP", Just '0'), ["ZI"])
           , (("ZP", Just '1'), [])
           , (("ZI", Just '0'), ["ZP"])
           , (("ZI", Just '1'), [])
           , (("UP", Just '1'), ["UI"])
           , (("UP", Just '0'), [])
           , (("UI", Just '0'), ["UP"])
           , (("UI", Just '1'), ["UP"])
           ]
           ["I"]
           ["ZP","UI"]
