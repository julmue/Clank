{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Semantics.Prop.LP
--
-- Semantics and truth-values of the classical propositional calculus __PC__.
--

module Logic.Semantics.Prop.PC
    ( V(F,T)
    , semantics
    ) where

import Prelude hiding (not, and, or, lookup)

import Logic.Semantics.Prop
import Logic.Data.Formula (Formula(Atom,Not,And,Or,Imp,Iff))

-- | Type of truth values of __PC__.
data V
    = F     -- ^ /False/
    | T    -- ^ /True/
    deriving (Show, Ord, Eq)

trvPC :: TrVals V
trvPC = makeTrVals [T,F] [T]

evalPC :: Formula V -> Formula V
evalPC fm = case fm of
    (Atom T) -> Atom T
    (Atom F) -> Atom F
    (Not p) -> not (evalPC p)
    (And p q) -> and (evalPC p) (evalPC q)
    (Or p q) -> or (evalPC p) (evalPC q)
    (Imp p q) -> imp (evalPC p) (evalPC q)
    (Iff p q) -> iff (evalPC p) (evalPC q)
    _ -> error "Error(eval): undefined input"

not :: Formula V -> Formula V
not (Atom p) = case p of
    T -> Atom F
    F -> Atom T
not _ = undefined

and :: Formula V -> Formula V -> Formula V
and (Atom p) aq@(Atom _) =
    case p of
    T -> aq
    F -> Atom F
and _ _ = undefined

or :: Formula V -> Formula V -> Formula V
or (Atom p) aq@(Atom _) =
    case p of
    T -> Atom T
    F -> aq
or _ _ = undefined

imp :: Formula V -> Formula V -> Formula V
imp (Atom p) (Atom q) =
    case p of
    T -> case q of
        T -> Atom T
        F -> Atom F
    F -> Atom T
imp _ _ = undefined

iff :: Formula V -> Formula V -> Formula V
iff ap@(Atom _) aq@(Atom _) = imp ap aq `and` imp aq ap
iff _ _ = undefined

-- | Semantics of LP
--
semantics :: Semantics V
semantics = makeSemantics trvPC evalPC
