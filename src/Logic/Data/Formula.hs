-- |
-- Module: Logic.Data.Formula
--
-- External interface of "Logic.Data.Formula".
--

module Logic.Data.Formula
    (
    -- * Types
      Formula (False, True, Atom, Not, And, Or, Imp, Iff)
    -- * Term destructor functions
    --
    -- Functions for deconstricting a 'Formula' based on its main connective.
    -- ! Attention: These functions are only partial so test before using!
    , destIff
    , antecedent
    , consequent
    , destAnd
    , conjuncts
    , destOr
    , disjuncts
    -- * Functions over 'Formula'.
    --
    -- These functions recursively process a 'Formula' f.
    , onFormulas
    , atomsSet
    , onAtoms
    , overAtoms
    -- * Substitution / Replacement - functions
    , simSubs
    , seqSubs
    , replace
    )
where

import Prelude hiding (Bool(False,True))

import Data.List
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- | The type for formulas.
data Formula a
    = False
    | True
    | Atom a
    | Not (Formula a)
    | And (Formula a) (Formula a)
    | Or (Formula a) (Formula a)
    | Imp (Formula a) (Formula a)
    | Iff (Formula a) (Formula a)
    deriving (Show, Eq)

{- Term destructor functions -}

-- | For a 'Formula' with the main connective 'Imp': a → b
-- this function returns a tuple (antecedent,consequent).
destIff :: Formula a -> (Formula a,Formula a)
destIff fm = case fm of
    (Iff p q)   -> (p,q)
    _           -> error "dest_iff: argument not an Iff"

-- | For a 'Formula' with the main connective 'Imp': a → b
-- this function returns the antecedent.
antecedent :: Formula a -> Formula a
antecedent fm = case fm of
    (Iff p _)   -> p
    _           -> error "antecedent: argument not an IFF"

-- | For a 'Formula' with the main connective 'Imp': a → b
-- this function returns the consequent.
consequent :: Formula a -> Formula a
consequent fm = case fm of
    (Iff _ q)   -> q
    _           -> error "antecedent: argument not an IFF"

-- | For a 'Formula' with the main connective 'And': a ∧ b
-- this function returns the tuple (a, b) of the two conjuncts.
destAnd :: Formula a -> (Formula a,Formula a)
destAnd fm = case fm of
    (And p q)   -> (p,q)
    _           -> error "dest_iff: argument not and And"

-- | For a 'Formula' with the main connective 'And': a ∧ b
-- this function returns the list [a, b] of the two conjuncts.
conjuncts :: Formula a -> [Formula a]
conjuncts fm = case fm of
    (And p q)   -> conjuncts p ++ conjuncts q
    _           -> [fm]

-- | For a 'Formula' with the main connective 'And': a ∨ b
-- this function returns the tuple (a, b) of the two disjuncts.
destOr :: Formula a -> (Formula a,Formula a)
destOr fm = case fm of
    (Or p q)   -> (p,q)
    _           -> error "dest_iff: argument not and Or"

-- | For a 'Formula' with the main connective 'And': a ∨ b
-- this function returns the list [a, b] of the two disjuncts.
disjuncts :: Formula a -> [Formula a]
disjuncts fm = case fm of
    (Or p q)   -> disjuncts p ++ disjuncts q
    _           -> [fm]

{- functions over formula -}

-- | 'onAtoms' applies a function to all atoms of
-- formula but leave structure untouched.
onAtoms :: (a -> b) -> Formula a -> Formula b
onAtoms f fm = case fm of
    True        -> True
    False       -> False
    Atom a      -> Atom $ f a
    Not p       -> Not $ onAtoms f p
    And p q     -> And (onAtoms f p) (onAtoms f q)
    Or p q      -> Or (onAtoms f p) (onAtoms f q)
    Imp p q     -> Imp (onAtoms f p) (onAtoms f q)
    Iff p q     -> Iff (onAtoms f p) (onAtoms f q)

-- | 'onFormulas' applies a function to every subformula of a 'Formula' f.
onFormulas :: (Formula a -> Formula a) -> Formula a -> Formula a
onFormulas f fm0 = case fm0 of
    True        -> f True
    False       -> f False
    Atom a      -> f $ Atom a
    Not p       -> Not $ f p
    And p q     -> And (f p) (f q)
    Or  p q     -> Or  (f p) (f q)
    Imp p q     -> Imp (f p) (f q)
    Iff p q     -> Iff (f p) (f q)

-- | 'overAtoms' takes a binary function
-- and applies it to the atoms in a 'Formula' f.
--
-- prominent use case: generate the list of all atoms in a Formula ('atomsSet').
--
-- !Attention: cornercases throw! Rework!!!
overAtoms :: (a -> b -> b) -> Formula a -> b -> b
overAtoms f fm b = case fm of
    True        -> undefined
    False       -> undefined
    Atom a      -> f a b
    Not p       -> overAtoms f p b
    And p q     -> overAtoms f p (overAtoms f q b)
    Or p q      -> overAtoms f p (overAtoms f q b)
    Imp p q     -> overAtoms f p (overAtoms f q b)
    Iff p q     -> overAtoms f p (overAtoms f q b)

-- | 'atomSet' returns the set of all atoms in a 'Formula' f.
atomsSet :: Eq a => Formula a -> [a]
atomsSet fm = nub $  overAtoms (:) fm []

{- Substitution functions -}

-- | Simultaneous substitution.
--
-- if more than one substitution is specified for the same atom
-- the first one in the list is applied.
--
-- seqSub [(α → β), (β → α)] α ∧ β ⇒ β ∧ α
--
simSubs :: (Eq a) => [(Formula a, Formula a)] -> Formula a -> Formula a
simSubs subList fm = case fm of
    True            -> substitution True
    False           -> substitution False
    atom@(Atom _)   -> substitution atom
    Not p           -> Not $ simSubs subList p
    And p q         -> And (simSubs subList p) (simSubs subList q)
    Or p q          -> Or (simSubs subList p) (simSubs subList q)
    Imp p q         -> Imp (simSubs subList p) (simSubs subList q)
    Iff p q         -> Iff (simSubs subList p) (simSubs subList q)
  where substitution f =
            let substitutions = [ subs | subs@(x,_) <- subList, x == f]
            in if null substitutions
               then f
               else snd . head $ substitutions

-- | Sequential substitution.
--
-- seqSub [(α → β), (β → α)] α ∧ β ⇒ α ∧ α
--
seqSubs :: Eq a => [(Formula a, Formula a)] -> Formula a -> Formula a
seqSubs subs fm = foldl (\ f sub -> simSubs [sub] f) fm subs

-- | Replacement.
--
-- This formula takes a function from 'Formula' to 'Formula' that should
-- be one implication a ⇒ b of a semantic equivalence a ⇔ b and replaces b for a
-- in a 'Formula' f.
--
replace :: (Formula a -> Formula a) -> Formula a -> Formula a
replace = onFormulas

-- | 'Formula' instance of Functor
instance Functor Formula where
    fmap = onAtoms

genFormula :: Arbitrary a => Int -> Gen (Formula a)
genFormula depth
    | depth <= 1 = oneof [Atom <$> arbitrary, elements [True, False]]
    | otherwise = do
        f1 <- genFormula <$> genDepth
        f2 <- genFormula <$> genDepth
        oneof [ genFormula 1
              , Not <$> f1
              , And <$> f1 <*> f2
              , Or <$> f1 <*> f2
              , Imp <$> f1 <*> f2
              , Iff <$> f1 <*> f2
              ]
    where genDepth = elements [1 .. pred depth]

instance Arbitrary a => Arbitrary (Formula a) where
    arbitrary = sized genFormula
