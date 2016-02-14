{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Semantics.Prop.L3
--
-- Semantics and truth-values of the logic __L3__ (/Lukasiewicz/).
--

module Logic.Semantics.Prop.L3
    ( V(F,I,T)
    , semantics
    ) where

import Prelude hiding (not, and, or, lookup, map)

import Logic.Semantics.Prop
import Logic.Data.Formula (Formula(Atom,Not,And,Or,Imp,Iff))

-- | Type of truth values of __L3__.
data V
    = F     -- ^ /False/
    | I     -- ^ Neither /True/ nor /False/
    | T     -- ^ /True/
    deriving (Show, Eq, Ord)

trvL3 :: TrVals V
trvL3 = makeTrVals [T,I,F] [T]

evalL3 :: Formula V -> Formula V
evalL3 fm = case fm of
    (Atom T) -> Atom T
    (Atom I) -> Atom I
    (Atom F) -> Atom F
    (Not p) -> not (evalL3 p)
    (And p q) -> and (evalL3 p) (evalL3 q)
    (Or p q) -> or (evalL3 p) (evalL3 q)
    (Imp p q) -> imp (evalL3 p) (evalL3 q)
    (Iff p q) -> iff (evalL3 p) (evalL3 q)
    _ -> error "Error(eval): undefined input"

not :: Formula V -> Formula V
not (Atom p) = case p of
    T  -> Atom F
    I  -> Atom I
    F  -> Atom T
not _ = undefined

and :: Formula V -> Formula V -> Formula V
and (Atom p) aq@(Atom q) =
    case p of
    T -> aq
    I -> case q of
        T -> Atom I
        I -> Atom I
        F -> Atom F
    F -> Atom F
and _ _ = undefined

or :: Formula V -> Formula V -> Formula V
or (Atom p) aq@(Atom q) =
    case p of
    T -> Atom T
    I -> case q of
        T -> Atom T
        I -> Atom I
        F -> Atom I
    F -> aq
or _ _ = undefined

imp :: Formula V -> Formula V -> Formula V
imp (Atom p) aq@(Atom q) =
    case p of
    T -> aq
    I -> case q of
        T -> Atom T
        I -> Atom T
        F -> Atom I
    F -> Atom T
imp _ _ = undefined

iff :: Formula V -> Formula V -> Formula V
iff ap@(Atom _) aq@(Atom _) = imp ap aq `and` imp aq ap
iff _ _ = undefined

-- | Semantics of K3
--
semantics :: Semantics V
semantics = makeSemantics trvL3 evalL3
