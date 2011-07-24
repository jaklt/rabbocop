{-# LANGUAGE CPP                      #-}
module Test.TestBitRepresentation
    ( testSteps
    ) where

import Control.Arrow
import Control.Monad
import Prelude
import Bits.BitRepresentation
import Helpers

testSteps :: IO ()
testSteps = do
        putStr "- test makeMove"
        let cases1 =
                [ ( "Rh1 rg1", [], "rg1 Rh1")
                , ( "Rd4", ["Rd4n", "Rd5n", "Rd6n"], "Rd7")
                , ( "Rc5", ["Rc5n"], "")
                , ( "Rb3 Ra3 Mf4 dg4 db2 re8"
                  , ["Rb3e", "db2n"]
                  , "Ra3 Mf4 dg4 db3 re8")
                , ( "Rc3 Cc4 rc5 rc6", ["rc5w", "Cc4n"], "rb5 Cc5")
                ]

        forM_ cases1 (\a@(b,steps,c) -> do
                        let b1 = parseBoard Gold b
                        let b2 = fst $ makeMove b1 $ map parseStep steps

                        when (b2 /= parseBoard Gold c)
                            (putStrLn $ ", " ++ show a ++ " failed")
                    )
        putStrLn "\t- DONE"

        putStr "- test canMakeStep"
        let cases2 =
                [ ("Rd4", ("Rd4n", ""), True)
                , ("Ra1 ra2", ("Ra1n", ""), False)
                , ("", ("Rb2n", ""), False)
                , ("Rd4 Re3 dd3", ("dd3n", "Re3w"), False)
                , ("Rd4 Rd3 de3", ("Rd3n", "de3w"), False)
                , ("Ra3 Rb3 Mf4 dg4 db2 re8" , ("Rb3e", "db2n"), True)
                , ("Ra3 Rb3 Mf4 dg4 db2 re8" , ("db2e", "Rb3s"), True)
                , ("Ra1 cb1", ("Ra1n", ""), False) -- frozen
                , ("Ra1 cb1 Db2", ("Ra1n", "cb1w"), False) -- frozen
                , ("Ra1 cb1 Db2", ("cb1e", "Ra1e"), False) -- frozen
                ]

        forM_ cases2 (\(b,(s1,s2), bo) -> do
                        let b' = parseBoard Gold b
                        unless (canMakeDStep b' (parseStep s1,parseStep s2)
                                == bo)
                            (putStrLn (", " ++ show (s1,s2,bo) ++ " failed")
                             >> printBoard b')
                    )
        putStrLn "\t- DONE"

        putStr "- test generateSteps"
        let cases3 =
                [ ( "Ra1", [("Ra1n",""), ("Ra1e","")], Gold, True)
                , ( "Rd5 ce5", [], Gold, True)
                , ( "Rd5 ce5"
                  , [("ce5n", ""), ("ce5e", ""), ("ce5s","")]
                  , Silver, False)
                , ( "Rd5 ce5"
                  , [ ("ce5n", ""), ("ce5e", ""), ("ce5s","")
                    , ("ce5n", "Rd5e"), ("ce5e", "Rd5e"), ("ce5s", "Rd5e")
                    , ("Rd5n", "ce5w"), ("Rd5s", "ce5w"), ("Rd5w", "ce5w")]
                  , Silver, True)
                , ( "Ca1 cb1", [("Ca1n", "")], Gold, True)
                , ( "Ca1 cb1", [("Ca1n", "")], Gold, False)
                , ( "Ca1 ra2 cb1", [("ra2n", "Ca1n"), ("ra2e", "Ca1n")]
                  , Gold, True)
                , ( "Ca1 ca2 cb1", [], Gold, True)
                ]

        forM_ cases3 (\(b, ss, pl, bo) -> do
                        let b' = parseBoard Gold b
                        let generated = generateSteps b' (pl, if bo then 0
                                                                    else 3)
                        let expected =
#ifdef canPass
                                [(Pass, Pass) | not bo] ++
#endif
                                map (parseStep *** parseStep) ss

                        unless (generated ~=~ expected)
                            (print generated
                             >> putStrLn (", " ++ show (ss,pl,bo) ++ " failed")
                             >> printBoard b')
                    )
        putStrLn "\t- DONE"

