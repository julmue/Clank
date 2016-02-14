{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Semantics.Internal
--
-- Internal interface of the "Logic.Semantics.Prop".
--

module Logic.Semantics.Prop.Internal
    (
    -- * Types
      Semantics (..)
    , TrVals(..)
    , Property(Valid, Sat, Unsat)
    -- * Constructor functions
    , makeTrVals
    , makeSemantics
    -- * Prototype functions
    , protoDomain
    , protoModels
    , protoModelsLookup
    , protoSat
    , protoValid
    , protoEntails
    -- * Utility functions
    , intersectModelLookups
    , extendModel
    , sortModels
    , association
    , combination
    , cartProd
    )
where

import Prelude hiding (lookup)

import Data.Function (on)
import Data.List ((\\), groupBy, sortBy, nub, intersect)
import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)

import Logic.Data.Formula hiding (True, False)
import Logic.Data.Prop

-- | The type of properties a given 'Formula' 'Prop' f can have with respect
-- to a 'Semantics' Τ s:
--
data Property
    = Valid         -- ^ 'Formula' 'Prop' f is valid in 'Semantics' s.
    | Sat           -- ^ 'Formula' 'Prop' f is satisfiable 'Semantics' s.
    | Unsat         -- ^ 'Formula' 'Prop' f is unsatisfiable 'Semantics' s.
    deriving (Show, Eq)

-- | A container type for truth-values
--
-- 'TrVals' combines the set of truth-values T and Α
-- (with A ⊂ T) where Α is the set of designated truth-values.
--
-- 'TrVals' is an abstract data type that should only be accessible through its
-- smart constructor 'makeTrVals'.
--
data TrVals a = TrVals
    { getTrVals :: [a]          -- ^ Accessor to the list of truth-values Τ.
    , getDesigTrVals :: [a]     -- ^ Accessor to the list of designated truth-values Α.
    } deriving (Show, Eq)

-- | Type for the class of many-valued semantics of propositional calculus.
--
data Semantics b = Semantics {
    -- | A set T of truth-values.
    trVals :: [b],
    -- | A set DT of designated truth-values.
    desigTrVals :: [b],
    -- | An evaluation function V : Formula T → Formula T
    -- that reduces Formula T of arbitrary length to a Formula T
    -- of structure 'Atom t' with t ∈ T.
    eval :: Formula b -> Formula b,
    -- | A function that maps a Formula Prop f to the set of its potential models PM.
    -- The assignment functions that are potential models of f have to be defined
    -- at least for every propositional variable p with p ∈ P(f).
    domain :: Formula Prop -> [Prop -> b],
    -- | A function that maps a Formula Prop f to the set of its models.
    -- The assignment functions that are models of of f have to be defined
    -- at least for every propositional variable p with p ∈ P(f).
    models :: Formula Prop -> [Prop -> b],
    -- | A function that protos the name-value-pairs of the models of a Formula Prop f explicit.
    -- The values returned by 'domain' and 'models' are 'raw' functions.
    modelsLookup :: Formula Prop -> [[(Prop,b)]],
    -- | A function that verifies validity of a Formula Prop f under a Semantics T.
    valid :: Formula Prop -> Bool,
    -- | A function that verifies satisfiability of a Formula Prop f under a Semantics T.
    sat :: Formula Prop -> Bool,
    -- | A function that verifies unsatisfiability of a Formula Prop f under a Semantics T.
    unsat :: Formula Prop -> Bool,
    -- | A function that verifies the entailment relation between a set of Formula Prop F
    -- and a Formula Prop f under a Semantics S T s.
    entails :: [Formula Prop] -> Formula Prop -> Bool
}

-- | Smart-constructor for truth-values.
-- It guarantees the following properties hold for every 'TrVals' α:
--
-- prop> getDesigTrVals α ⊂ getTrVals α
makeTrVals :: Eq a => [a] -> [a] -> TrVals a
makeTrVals vals desigVals =
    case nub desigVals \\ nub vals of
    [] -> TrVals vals desigVals
    _ -> error "Error(protoTrVals): Set of truth values isn't superset of set of designated Truth values!"

-- | Constructor for a generic many-values propositional semantics 'Semantics' T over truth-values T.
-- This constructor takes two arguments:
--
--     (1) A set of truth-values Τ and designated truth values Α ⊂ Τ (combined in a type 'TrVals' Τ).
--     (2) An evaluation function ε :: 'Formula' Τ -> 'Formula' Τ that reduces a 'Formula' Τ of arbitrary length
--         to a 'Formula' Τ of structure @'Atom' τ@ with τ ∈ Τ.
--
--     ε has to be defined for the constructors: 'Atom', 'Not', 'And', 'Or', 'Imp', 'Iff' of type 'Formula'.
--
--
makeSemantics :: Eq b =>
    TrVals b                            -- ^ The truth-value-Argument e.g. 'makeTrVals' [T,F] [F] (assuming there is a data V = T | F)
    -> (Formula b -> Formula b)         -- ^ An evaluation formula for Formula V; it has to be defined for Atom, Not, And, Or, Imp, Iff.
    -> Semantics b                      -- ^ The generated Semantics.
makeSemantics tvs evalFn = Semantics
    { trVals = getTrVals tvs
    , desigTrVals = getDesigTrVals tvs
    , eval = evalFn
    , domain = protoDomain tvs
    , models = protoModels tvs evalFn
    , modelsLookup = protoModelsLookup tvs evalFn
    , unsat = not . protoSat tvs evalFn
    , sat = protoSat tvs evalFn
    , valid = protoValid tvs evalFn
    , entails = protoEntails tvs evalFn
    }

-- | 'protoDomain' is a generic prototype for 'domain'.
--
-- When given a set of truth-values Τ and designated truth-values Δ ⊂ Τ (wrapped in 'TrVals')
-- 'protoDomain' returns a function δ with signature:
--
-- δ :: 'Formula' 'Prop' -> 'Prop' -> Τ
--
-- that generates all possible models\/assignment functions Α :: 'Prop' -> T for a 'Formula' 'Prop' f
-- based on set of propositional variables appearing in f.
--
-- δ is only a partial function: it's undefined for propositional variables that don't appear in f.
--
protoDomain :: TrVals b -> Formula Prop -> [Prop -> b]
protoDomain tvs fm =
    protoAssignmentFn <$> lookupTables
  where protoAssignmentFn :: [(Prop, b)] -> Prop -> b
        protoAssignmentFn lookupTable a =
            let m = fromList lookupTable
            in fromMaybe (error "Error(Assignment): variable not in assignment function")
               (lookup a m)
        pairsAtomValue = cartProd (atomsSet fm) (getTrVals tvs)
        partitionsByAtom = groupBy ((==) `on` fst) pairsAtomValue
        lookupTables = combination partitionsByAtom

-- | 'protoModels' is a generic prototype for 'models'.
--
-- When given a set of truth-values Τ and designated truth-values Δ ⊂ Τ (wrapped in 'TrVals')
-- and an evaluation function ε :: Formula T -> Formula T
-- 'protoModels' returns a function δ with signature:
--
-- δ :: 'Formula' 'Prop' -> 'Prop' -> Τ
--
-- that generates all models\/assignment functions Α :: 'Prop' -> T for a 'Formula' 'Prop' f
-- based on set of propositional variables appearing in f.
--
-- δ is only a partial function: it's undefined for propositional variables that don't appear in f.
--
protoModels :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> [Prop -> b]
protoModels ts evalFn fm =
    let d = protoDomain ts fm
        filt = flip elem (Atom <$> getDesigTrVals ts)
        mask = map filt ((evalFn . ($ fm) . onAtoms) <$> d)
    in [ m | (m, True) <- d `zip` mask]

-- | 'protoModels' is a generic prototype for 'modelsLookup'.
--
-- When given a set of truth-values Τ and designated truth-values Δ ⊂ Τ (wrapped in 'TrVals')
-- and an evaluation function ε :: Formula T -> Formula T
-- 'protoModelsLookup' returns a function δ with signature:
--
-- δ :: 'Formula' 'Prop' -> 'Prop' -> [[(Prop,T)]]
--
-- that generates the name-value-pairs (lookup-tables) of all models\/assignment functions
-- of a 'Formula' 'Prop' f based on set of propositional variables appearing in f.
--
protoModelsLookup :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> [[(Prop, b)]]
protoModelsLookup ts evalFn fm =
    let as = atomsSet fm
        ms = protoModels ts evalFn fm
    in map (zip as . ($ as) . map) ms

-- | 'protoSat' is a generic prototype for 'sat'
--
-- When given a set of truth-values Τ and designated truth-values Δ ⊂ Τ (wrapped in 'TrVals')
-- and an evaluation function ε :: Formula T -> Formula T
-- 'protoSat' returns a function δ with signature:
--
-- δ :: 'Formula' 'Prop' -> 'Prop' -> Bool
--
-- that test a 'Formula' 'Prop' f for satisfiability.
--
protoSat :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> Bool
protoSat tvs evalFn fm = (not . null) (protoModels tvs evalFn fm)

-- | 'protoValid' is a generic prototype for 'valid'
--
-- When given a set of truth-values Τ and designated truth-values Δ ⊂ Τ (wrapped in 'TrVals')
-- and an evaluation function ε :: Formula T -> Formula T
-- 'protoSat' returns a function δ with signature:
--
-- δ :: 'Formula' 'Prop' -> 'Prop' -> Bool
--
-- that test a 'Formula' 'Prop' f for validity.
--
protoValid :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> Bool
protoValid tvs evalFn fm = length (protoModels tvs evalFn fm) == length (protoDomain tvs fm)

-- | 'protoEntails' is a generic prototype for 'entails'
--
-- | 'protoValid' is a generic prototype for 'valid'
--
-- When given a set of truth-values Τ and designated truth-values Δ ⊂ Τ (wrapped in 'TrVals')
-- and an evaluation function ε :: Formula T -> Formula T
-- 'protoSat' returns a function δ with signature:
--
-- δ :: ['Formula' 'Prop'] -> 'Formula' 'Prop' -> 'Prop' -> Bool
--
-- that test if a 'Formula' 'Prop' f is entailed by a set of 'Formula' 'Prop' F.
--
-- Its implementation is naive:
--
--      (1) It generates all models of f := msfm.
--      (2) It generates all models of F := msfms.
--      (3) It refines msfm and msfms that all models contain
--          the same set of propositional variables.
--      (4) It checks if msfms ⊆ msfm
--
protoEntails :: Eq b =>
     TrVals b -> (Formula b -> Formula b) -> [Formula Prop] -> Formula Prop -> Bool
protoEntails tvs evalFn fms fm =
    let modelsFm = protoModelsLookup tvs evalFn fm
        allModelsFms = map (protoModelsLookup tvs evalFn) fms
        modelsFms = intersectModelLookups allModelsFms
        atomsInFm = nub $ concat $ (fmap . fmap) fst modelsFm
        atomsInFms =  nub $ concat $ (fmap . fmap) fst modelsFms
        atoms = nub $ atomsInFms ++ atomsInFm
        extModelsFm = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFm
        extModelsFms = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFms
    in case extModelsFms\\extModelsFm of
        [] -> True
        _ -> False

-- | 'intersectModelLookups' generates the intersection of a set of
-- pair-value lists.
intersectModelLookups :: Eq b => [[[(Prop, b)]]] -> [[(Prop, b)]]
intersectModelLookups ms = case ms of
     [] -> []
     x -> foldr1 intersect $ fmap sortModels x

-- | 'extendModel' is
extendModel :: TrVals b -> [Prop] -> [(Prop, b)] -> [[(Prop, b)]]
extendModel tvs atoms mlookups =
    let as = nub atoms
        msAtoms = nub $ fmap fst mlookups
        atomsOnly = as \\ msAtoms
        extensions = sequence $ association atomsOnly (getTrVals tvs)
    in  fmap(mlookups ++) extensions

-- | lexicographical sort of model-lookup-tables by the name of the propositional variable.
sortModels :: [[(Prop, b)]] -> [[(Prop, b)]]
sortModels =
    map (sortBy (\(x,_) (y,_) -> x `compare` y))

-- | ! ToDo !
association :: Functor f => f Prop -> [b] -> f [(Prop, b)]
association l1 l2 =
    fmap (($ l2) . (<*>) . fmap (,) . pure) l1

-- | This function generates the combination of list of lists.
-- example:
--
-- >>> combination [[1,2],[3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]
--
-- possible replacement: sequence xs
combination :: [[a]] -> [[a]]
combination [] = []
combination [as] = fmap (:[]) as
combination (x:xs) = (:) <$> x <*> combination xs

-- | This function generates the cartesian product of two lists.
-- example:
--
-- >>> cartProd [1,2] ['a','b']
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
cartProd :: [a] -> [b] -> [(a,b)]
cartProd as bs = (,) <$> as <*> bs
