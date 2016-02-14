{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Semantics.Prop.RM
--
-- Semantics and truth-values of the logic __RM__.
--

module Logic.Semantics.Prop.RM
    ( V(F,I,T)
    , semantics
    ) where

import Prelude hiding (not, and, or, lookup, map)

import Logic.Semantics.Prop
import Logic.Data.Formula (Formula(Atom,Not,And,Or,Imp,Iff))

-- | Type of truth values of __RM__.
data V
    = F     -- ^ /False/
    | I     -- ^ /True/ and /False/ see: <http://plato.stanford.edu/entries/dialetheism/ dialethism>
    | T     -- ^ /True/
    deriving (Show, Eq, Ord)

trvRM :: TrVals V
trvRM = makeTrVals [T,I,F] [T,I]

evalRM :: Formula V -> Formula V
evalRM fm = case fm of
    (Atom T) -> Atom T
    (Atom I) -> Atom I
    (Atom F) -> Atom F
    (Not p) -> not (evalRM p)
    (And p q) -> and (evalRM p) (evalRM q)
    (Or p q) -> or (evalRM p) (evalRM q)
    (Imp p q) -> imp (evalRM p) (evalRM q)
    (Iff p q) -> iff (evalRM p) (evalRM q)
    _ -> error "Error(evalRM): undefined input"

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
imp (Atom p) (Atom q) =
    case p of
    T -> case q of
        T -> Atom T
        _ -> Atom F
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
semantics = makeSemantics trvRM evalRM
