{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Semantics.Prop.LP
--
-- Semantics and truth-values of the logic __LP__ (/Priest/).
--

module Logic.Semantics.Prop.LP
    ( V(F,I,T)
    , semantics
    ) where

import Prelude hiding (not, and, or, lookup, map)

import Logic.Semantics.Prop
import Logic.Data.Formula (Formula(Atom,Not,And,Or,Imp,Iff))

-- | Type of truth values of __LP__.
data V
    = F     -- ^ /False/
    | I     -- ^ /True/ and /False/ see: <http://plato.stanford.edu/entries/dialetheism/ dialethism>
    | T     -- ^ /True/
    deriving (Show, Eq, Ord)

trvLP :: TrVals V
trvLP = makeTrVals [T,I,F] [T,I]

evalLP :: Formula V -> Formula V
evalLP fm = case fm of
    (Atom T) -> Atom T
    (Atom I) -> Atom I
    (Atom F) -> Atom F
    (Not p) -> not (evalLP p)
    (And p q) -> and (evalLP p) (evalLP q)
    (Or p q) -> or (evalLP p) (evalLP q)
    (Imp p q) -> imp (evalLP p) (evalLP q)
    (Iff p q) -> iff (evalLP p) (evalLP q)
    _ -> error "Error(evalLP): undefined input"

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
        I -> Atom I
        F -> Atom I
    F -> Atom T
imp _ _ = undefined

iff :: Formula V -> Formula V -> Formula V
iff ap@(Atom _) aq@(Atom _) = imp ap aq `and` imp aq ap
iff _ _ = undefined

-- | Semantics of LP
--
semantics :: Semantics V
semantics = makeSemantics trvLP evalLP


