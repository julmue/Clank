{-# OPTIONS_GHC -Wall -Werror #-}

module Clank.Parser.Task
    ( Error,
      toTasks
    ) where

import Control.Applicative

import Clank.Data.Task

{-  Simple backtracking Parser to operate on a stream of strings;
    for parsing the return value of getArgs.
    Maybe this can be rewritten as a ParsecT parser ...
-}

type Error = String
type Args = [String]
type ParserTok a = Parser [String] a

newtype Parser s a = Parser { runParser :: s -> (s, Either Error a) }

instance Functor (Parser s) where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser pa) = Parser $
        \s0 -> case pa s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right a) -> (s1, Right $ f a)

instance Applicative (Parser s) where
    -- pure :: a -> (Parser s) a
    pure a = Parser $
        \s0 -> (s0, Right a)
    -- (apply) <*> :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pfab) <*> (Parser pa) = Parser $
        \s0 -> case pfab s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right fab) -> case pa s1 of
                (_, Left err) -> (s0, Left err)
                (s2, Right a) -> (s2, Right $ fab a)

instance Alternative (Parser s) where
    -- empty :: (Parser s) a
    empty = Parser $ \s0 -> (s0, Left "empty")
    -- (choice/alternative) (<|>) :: Parser a -> Parser a -> Parser a
    (Parser pa1) <|> (Parser pa2) = Parser $
        \s0 -> case pa1 s0 of
            resp1@(_, Right _) -> resp1
            (_, Left _) -> case pa2 s0 of
                resp2@(_, Right _) -> resp2
                (_, Left err) -> (s0, Left err)
    -- some: 1 or more
    -- some :: (Parser s) a -> (Parser s) [a]
    some p@(Parser pa) = Parser $
        \s0 -> case pa s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right a) -> case (runParser $ many p) s1 of
                (_, Left _) -> (s1, Right [a])
                (s2, Right as) -> (s2, Right $ a:as)
    -- many: 0 or more
    -- many :: (Parser s) a -> (Parser s) [a]
    many p@(Parser pa) = Parser $
        \s0 -> case pa s0 of
            (_, Left _) -> (s0, Right [])
            (s1, Right a) -> case (runParser $ some p) s1 of
                (_, Left _) -> (s1, Right [a])
                (s2, Right as) -> (s2, Right $ a:as)

instance Monad (Parser s) where
    -- return :: a -> (Parser s) a
    return = pure
    -- (bind) (>>=) :: (Parser s) a -> (a -> (Parser s) b) -> Parser b
    (Parser pa) >>= fab = Parser $
        \s0 -> case pa s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right a) -> runParser (fab a) s1


satisfy :: (s -> Bool) -> Parser [s] s
satisfy f = Parser $
    \s0 -> case s0 of
        [] -> (s0, Left "Error: end of stream")
        (s:s1) -> if f s
                  then (s1, Right s)
                  else (s0, Left "Error: did not satisfy")

oneOf :: [String] -> ParserTok String
oneOf sl = satisfy (`elem` sl)

noneOf :: [String] -> ParserTok String
noneOf sl = satisfy $ not . flip elem sl

flagFormula :: [String]
flagFormula = ["-f","--formula"]

flagSemantics :: [String]
flagSemantics = ["-s","--semantics"]

flagTurnstile :: [String]
flagTurnstile = ["-t","--turnstile","--entails"]

flagClassification :: [String]
flagClassification = ["-c","--classify"]

flagProperty :: [String]
flagProperty = ["-p","--properties"]

flagModel :: [String]
flagModel = ["-m","-ms","--models","--model"]

flagNormalForm :: [String]
flagNormalForm = ["-n","--normalform"]

flagHelp :: [String]
flagHelp = ["-h","--help"]

flags :: [String]
flags = flagFormula ++ flagSemantics ++ flagTurnstile ++ flagClassification ++ flagProperty ++
        flagModel ++ flagNormalForm ++ flagHelp

-- scanOptions
-- scans a stream of argument tokens for flag and flag-options until the next flag
-- can also be used to scan for switches: return (Left err) if a flag is not found
-- there should be a way to distinguish between flags with not arguments (switches
-- and flags that take arguments (Options)
scanOptions :: [String] -> [String] -> Parser [String] [String]
scanOptions flgs flg =
    ((++) <$> options flgs flg <*> scanOptions flgs flg)
    <|> options flgs flg
    where options fls fl = many (noneOf fl) *> oneOf fl *> many (noneOf fls)

scanWithFlags :: [String] -> Parser [String] [String]
scanWithFlags = scanOptions flags

-- scanners for the flags
-- switches return Left err on failure
scanFormulas :: Parser [String] [String]
scanFormulas = scanWithFlags flagFormula

scanSemantics :: Parser [String] [String]
scanSemantics = scanWithFlags flagSemantics

scanTurnstile :: Parser [String] [String]
scanTurnstile = scanWithFlags flagTurnstile

scanClassification :: Parser [String] [String]
scanClassification = scanWithFlags flagClassification

scanProperties :: Parser [String] [String]
scanProperties = scanWithFlags flagProperty

scanModels :: Parser [String] [String]
scanModels = scanWithFlags flagModel

scanNormalForms :: Parser [String] [String]
scanNormalForms = scanWithFlags flagNormalForm

scanHelp :: Parser [String] [String]
scanHelp = scanWithFlags flagHelp

toSemantics :: String -> Either Error SemanticsReq
toSemantics s = case s of
    "pc"    -> Right PCReq
    "l3"    -> Right L3Req
    "k3"    -> Right K3Req
    "lp"    -> Right LPReq
    "rm"    -> Right RMReq
    x       -> Left $ "Error(toSemantics): " ++ x ++ " is not a known semantics!"

toProperty :: String -> Either Error PropertyReq
toProperty s = case s of
    "valid" -> Right ValidReq
    "sat"   -> Right SatReq
    "unsat" -> Right UnsatReq
    x       -> Left $ "Error(toProperty): " ++ x ++ " is not a known porperty!"

toNormalForms :: String -> Either Error NormalFormReq
toNormalForms s = case s of
    "cnf"   -> Right CNFReq
    "dnf"   -> Right DNFReq
    x       -> Left $ "Error(toNormalForm): " ++ x ++ " is not a known normal form!"

-- error handling has to be refined ...
getFormulaReqs :: [String] -> Either Error [String]
getFormulaReqs s = case getArgs scanFormulas s of
    (Left _)        -> Left "Error(getFormulaReqs): No formulas specified"
    (Right [])      -> Left "Error(getFormulaReqs): No formulas specified"
    (Right fs)      -> Right fs

getSemanticReqs :: [String] -> Either Error [SemanticsReq]
getSemanticReqs s = case getArgs scanSemantics s of
    (Left _)        -> Left "Error(getSemanticReqs): No semantics specified"
    (Right [])      -> Left "Error(getSemanticReqs): No semantics specified"
    (Right ss)      -> sequence $ fmap toSemantics ss

getTurnstileReqs :: [String] -> Either Error Bool
getTurnstileReqs s = case getArgs scanTurnstile s of
    (Left _)        -> Right False
    (Right [])      -> Right True
    (Right x)       -> Left $ "Error(getTurnstyle): unknown Argument(s): " ++ concat (fmap show x)

getClassificationReqs :: [String] -> Either Error Bool
getClassificationReqs s = case getArgs scanClassification s of
    (Left _)        -> Right False
    (Right [])      -> Right True
    (Right x)       -> Left $ "Error(getClassificationReqs): unknown Argument(s): " ++ concat (fmap show x)

getPropertyReqs :: [String] -> Either Error [PropertyReq]
getPropertyReqs s = case getArgs scanProperties s of
    (Left _)        -> Right []
    (Right x)       -> sequence $ fmap toProperty x

getModelReqs :: [String] -> Either Error Bool
getModelReqs s = case getArgs scanModels s of
    (Left _)        -> Right False
    (Right [])      -> Right True
    (Right x)       -> Left $ "Error(getModelReqs): unknown Argument(s): " ++ concat (fmap show x)

getNormalFormReqs :: [String] -> Either Error [NormalFormReq]
getNormalFormReqs s = case getArgs scanNormalForms s of
    (Left _)        -> Right []
    (Right x)       -> sequence $ fmap toNormalForms x

getHelpReqs :: [String] -> Either Error Bool
getHelpReqs s = case getArgs scanHelp s of
    (Left _)        -> Right False
    (Right [])      -> Right True
    (Right _)       -> Right True

getArgs :: Parser s a -> s -> Either Error a
getArgs p s = snd $ runParser p s

requestConstructor :: Args -> Either Error Request
requestConstructor args =
    pure Request
    <*> getFormulaReqs args
    <*> getSemanticReqs args
    <*> getTurnstileReqs args
    <*> getClassificationReqs args
    <*> getPropertyReqs args
    <*> getModelReqs args
    <*> getNormalFormReqs args
    <*> getHelpReqs args


tasksConstructor :: Request -> [Task]
tasksConstructor req =
    let fs@(hf:tf) = getReqFormulas req
        ss = getReqSemantics req
        ps = getReqProperties req
        nfs = getReqNormalForms req
    in  [ Task hf s (TurnstileAction tf)    | s <- ss, getReqTurnstile req ] ++
        [ Task f s ClassifyAction           | f <- fs, s <- ss, getReqClassify req] ++
        [ Task f s (PropertyAction p)       | f <- fs, s <- ss, p <- ps ] ++
        [ Task f s ModelAction              | f <- fs, s <- ss, getReqModels req ] ++
        [ Task f s (NFAction nf)            | f <- fs, s <- ss, nf <- nfs ] ++
        [ Task [] PCReq HelpAction          | getReqHelp req ]

toTasks :: Args -> Either Error [Task]
toTasks args = case tasksConstructor <$> requestConstructor args of
    -- VVV This can only be preliminary ... sensible error messages are necessary ...
    -- under no condition should the error handling happen here ...
    (Left _)        -> Right [ Task [] PCReq HelpAction ]
    (Right [])      -> Right [ Task [] PCReq HelpAction ]
    x               -> x
