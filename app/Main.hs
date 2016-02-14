-- {-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE ExistentialQuantification #-}

import System.Environment (getArgs)

import Clank.Parser.Task
import Clank.Process.Tasks


main :: IO ()
main = do
    args <- getArgs
    case toTasks args of
        (Left err) -> print err
        (Right tasks) -> mapM_ putStrLn $ processTasks tasks

{- ToDo:
    Languages of Formulas are not properly linked to the semantics ...
    as of now it is possible to attempt an evaluation of a LPC formula
    under PC semantics -- which throws.

    It should be possible to derive the set of languages a formula belongs to
    e.g.:   > derive "a && b": pc, lpc, l3, s4, ...
            > derive "Forall.a: Px": lpc, ...
    and return sensible error messages according to this classification.
-}
