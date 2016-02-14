{-# OPTIONS_GHC -Wall -Werror #-}

module Clank.Process.Tasks (
    processTasks
    ) where

import Data.List (intersperse)

import Text.Parsec (parse)

import Logic.Data.Formula hiding (True,False)
import Logic.Parser.Prop
import Logic.Data.Prop
import Logic.Semantics.Prop
import Logic.Semantics.Prop.PC as PC
import Logic.Semantics.Prop.K3 as K3
import Logic.Semantics.Prop.L3 as L3
import Logic.Semantics.Prop.LP as LP
import Logic.Semantics.Prop.RM as RM
import Clank.Data.Task
import Clank.Data.ShowBox (ShowBox(..))

processTasks :: [Task] -> [String]
processTasks = fmap processTask

processTask :: Task -> String
processTask t =
    let s = getTaskFormula t
        sem = getTaskSemantics t
    in  case getTaskAction t of
        TurnstileAction ss      -> show $ unpack $ getTurnstile sem s ss
        ClassifyAction          -> show $ unpack $ getClassification sem s
        (PropertyAction pa)     -> show $ unpack $ getProperty sem s pa
        (ModelAction)           -> show $ unpack $ getModels sem s
        (NFAction _)            -> error "not yet defined"
        (HelpAction)            -> help
  where unpack result = case result of
            (Left err) -> ShowBox err
            (Right value) -> ShowBox value

{- entailment relation -}
getTurnstile :: SemanticsReq -> String -> [String] -> Either String Bool
getTurnstile sem s ss =
    case sem of
    PCReq -> turnstile PC.semantics s ss
    L3Req -> turnstile L3.semantics s ss
    K3Req -> turnstile K3.semantics s ss
    LPReq -> turnstile LP.semantics s ss
    RMReq -> turnstile RM.semantics s ss

turnstile :: Semantics b -> String -> [String] -> Either String Bool
turnstile sem s ss =
    case parse formulaProp "" s of
    (Left err) -> Left $ show err
    (Right f) -> case ss of
        [] -> Right $ valid sem f
        _ -> case sequence $ fmap (parse formulaProp "") ss of
            (Left err) -> Left $ show err
            (Right fs) -> Right $ entails sem fs f

{- formula cassifications -}
getClassification :: SemanticsReq -> String -> Either String Property
getClassification sem s =
    case sem of
    PCReq -> classificationPC s
    L3Req -> classificationL3 s
    K3Req -> classificationK3 s
    LPReq -> classificationLP s
    RMReq -> classificationRM s

makeClassification :: Semantics b -> String -> Either String Property
makeClassification sem s =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Classification Propositional Calculus:" ++ show err
    (Right f) -> Right $ if sat sem f
                         then if valid sem f
                              then Valid
                              else Sat
                         else Unsat

classificationPC :: String -> Either String Property
classificationPC = makeClassification PC.semantics

classificationK3 :: String -> Either String Property
classificationK3 = makeClassification K3.semantics

classificationL3 :: String -> Either String Property
classificationL3 = makeClassification L3.semantics

classificationLP :: String -> Either String Property
classificationLP = makeClassification LP.semantics

classificationRM :: String -> Either String Property
classificationRM = makeClassification RM.semantics


{- property tasks -}
getProperty :: SemanticsReq -> String -> PropertyReq -> Either String Bool
getProperty sem s pa =
    case sem of
    PCReq -> propertyPC s pa
    L3Req -> propertyL3 s pa
    K3Req -> propertyK3 s pa
    LPReq -> propertyLP s pa
    RMReq -> propertyRM s pa

makeProperty :: Semantics b -> String -> PropertyReq -> Either String Bool
makeProperty sem s pa =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Property Propositional Calculus:" ++ show err
    (Right f) -> Right $ case pa of
        ValidReq -> valid sem f
        SatReq -> sat sem f
        UnsatReq -> unsat sem f

propertyPC :: String -> PropertyReq -> Either String Bool
propertyPC = makeProperty PC.semantics

propertyK3 :: String -> PropertyReq -> Either String Bool
propertyK3 = makeProperty K3.semantics

propertyL3 :: String -> PropertyReq -> Either String Bool
propertyL3 = makeProperty L3.semantics

propertyLP :: String -> PropertyReq -> Either String Bool
propertyLP = makeProperty LP.semantics

propertyRM :: String -> PropertyReq -> Either String Bool
propertyRM = makeProperty LP.semantics


{- model task -}
getModels :: SemanticsReq -> String -> Either String String
getModels sem s =
    case parse formulaProp "" s of
        (Left err) -> Left $ "Model Propositional Calculus:" ++ show err
        (Right f) -> Right $ case sem of
            PCReq -> show $ showModelsPC f
            K3Req -> show $ showModelsK3 f
            L3Req -> show $ showModelsL3 f
            LPReq -> show $ showModelsLP f
            RMReq -> show $ showModelsRM f

showModelsPC :: Formula Prop -> [[(Prop, PC.V)]]
showModelsPC = modelsLookup PC.semantics

showModelsK3 :: Formula Prop -> [[(Prop, K3.V)]]
showModelsK3 = modelsLookup K3.semantics

showModelsL3 :: Formula Prop -> [[(Prop, L3.V)]]
showModelsL3 = modelsLookup L3.semantics

showModelsLP :: Formula Prop -> [[(Prop, LP.V)]]
showModelsLP = modelsLookup LP.semantics

showModelsRM :: Formula Prop -> [[(Prop, RM.V)]]
showModelsRM = modelsLookup RM.semantics

{- Help -}
help :: String
help = unlines $ [ "usage: clank [Option1] [Args1] [Option2] [Args2] ... " ] ++
       ( optToList $ Option
                        { optName = "Formula"
                        , optRequired = True
                        , optFlags = ["-f","--formula"]
                        , optArgs = ["[Formula1] [Formula2] ..."]
                        , optSynopsis = "One or more Formulas"
                        , optExample = "-f 'p -> (p -> q) -> q' ..."
                        }
        ) ++
        ( optToList $ Option
                        { optName = "Semantics"
                        , optRequired = True
                        , optFlags = ["-s","--semantics"]
                        , optArgs = [ "pc (Propositional Calculus)"
                                    , "k3 (Strong Kleene)"
                                    , "l3 (Lukasiewicz"
                                    , "lp (Priest)"
                                    , "rm RM"
                                    ]
                        , optSynopsis = "One or more Semantics pc"
                        , optExample = "-f <formula1> <formula2> ... -s pc"
                        }
        ) ++
        ( optToList $ Option
                        { optName = "Classification"
                        , optRequired = False
                        , optFlags = ["-c","--classification"]
                        , optArgs = [ "NONE"]
                        , optSynopsis = "classification of a formula - returns 'Valid', 'Sat' or 'Unsat'"
                        , optExample = "-f <formula1> <formula2> ... -s pc -c"
                        }
        ) ++
        ( optToList $ Option
                        { optName = "Property Assertion"
                        , optRequired = False
                        , optFlags = ["-p","--property"]
                        , optArgs = [ "valid","sat","unsat"]
                        , optSynopsis = "Property assertion of a formula - returns 'True' of 'False'"
                        , optExample = "-f <formula1> <formula2> ... -s pc -p valid"
                        }
        ) ++
        ( optToList $ Option
                        { optName = "Models"
                        , optRequired = False
                        , optFlags = ["-ms","--models"]
                        , optArgs = [ "NONE"]
                        , optSynopsis = "returns the models of a formula as (name,value) pairs"
                        , optExample = "-f <formula1> <formula2> ... -s pc -m"
                        }
        ) ++
               ( optToList $ Option
                        { optName = "Normalform"
                        , optRequired = False
                        , optFlags = ["-n","--normalform"]
                        , optArgs = [ "not yet implemented"]
                        , optSynopsis = "returns a formula in a specified normal form"
                        , optExample = "-f <formula1> <formula2> ... -s pc -m"
                        }
        ) ++
       ( optToList $ Option
                        { optName = "Entailment"
                        , optRequired = False
                        , optFlags = ["-t","--turnstile","--entails"]
                        , optArgs = [ "NONE"]
                        , optSynopsis = "returns if <formula1> is entailed by the set <formula2> <formula3> ..."
                        , optExample = "-f <formula1> <formula2> ... -s -t"
                        }
        ) ++
       ( optToList $ Option
                        { optName = "Help"
                        , optRequired = False
                        , optFlags = ["-h","--help"]
                        , optArgs = [ "NONE"]
                        , optSynopsis = "The help file ... you're just reading it ;-) "
                        , optExample = "-h"
                        }
        )

data Option = Option
    {   optName :: String
    ,   optRequired :: Bool
    ,   optFlags :: [String]
    ,   optArgs :: [String]
    ,   optSynopsis :: String
    ,   optExample :: String
    }

optToList :: Option -> [String]
optToList opt =
    let pad11 = 4
        pad12 = 24
        pad21 = 8
        pad22 = 28
    in  [ padding pad11 status          pad12 (optName opt)
        , padding pad21 "Flags:"        pad22 ((concat . intersperse ", ") $ optFlags opt)
        , padding pad21 "Arguments:"    pad22 ((concat . intersperse ", ") $ optArgs opt)
        , padding pad21 "Synopsis:"     pad22 (optSynopsis opt)
        , padding pad21 "Example:"      pad22 (optExample opt)
        ]
  where
    status = if optRequired opt
             then "Mandatory Option:"
             else "Option:"
    padding :: Int -> [Char] -> Int -> [Char] -> [Char]
    padding margin1 left margin2 right =
        let offset = margin2 - length left - margin1
        in replicate margin1 ' ' ++ left ++ replicate offset ' ' ++ right
