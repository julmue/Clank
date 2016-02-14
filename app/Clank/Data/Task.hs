{-# OPTIONS_GHC -Wall -Werror #-}

module Clank.Data.Task
    ( Request(..)
    , Task(..)
    , Action(..)
    , SemanticsReq(..)
    , PropertyReq(..)
    , NormalFormReq(..)
    ) where

data Request = Request {
    getReqFormulas          :: [String],
    getReqSemantics         :: [SemanticsReq],
    getReqTurnstile         :: Bool,
    getReqClassify          :: Bool,
    getReqProperties        :: [PropertyReq],
    getReqModels            :: Bool,
    getReqNormalForms       :: [NormalFormReq],
    getReqHelp              :: Bool
} deriving (Show,Eq)

data Task = Task {
    getTaskFormula          :: String,
    getTaskSemantics        :: SemanticsReq,
    getTaskAction           :: Action
} deriving (Show,Eq)

data Action
    = TurnstileAction [String]
    | ClassifyAction
    | PropertyAction PropertyReq
    | ModelAction
    | NFAction  NormalFormReq
    | HelpAction
    deriving (Show,Eq)

-- the different form of queries are classified by their answer types:
-- classify :: FProp = Valid, Sat, Unsat
-- sat, unsat, valid :: Bool
-- model, models :: [Model]
-- normalform :: Formula

data SemanticsReq
    = PCReq
    | K3Req
    | L3Req
    | LPReq
    | RMReq
    deriving (Show,Eq)

data PropertyReq
    = ValidReq
    | SatReq
    | UnsatReq
    deriving (Show,Eq)

data NormalFormReq
    = CNFReq
    | DNFReq
    deriving (Show,Eq)


