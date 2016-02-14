{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Semantics.Prop.K3
--
-- Semantics and truth-values of the logic __K3__ (/Strong Kleene/).
--

module Logic.Semantics.Prop.K3
    (
    -- * Truth Values
    V(F,I,T)
    -- * Semantics of K3
    , semantics
    ) where

import Prelude hiding (not, and, or, lookup, map)

import Logic.Semantics.Prop
import Logic.Data.Formula (Formula(Atom,Not,And,Or,Imp,Iff))

-- | Type of truth values of __K3__.
data V
    = F     -- ^ /False/
    | I     -- ^ Neither /True/ nor /False/
    | T     -- ^ /True/
    deriving (Show, Eq, Ord)

trvK3 :: TrVals V
trvK3 = makeTrVals [T,I,F] [T]

evalK3 :: Formula V -> Formula V
evalK3 fm = case fm of
    (Atom T) -> Atom T
    (Atom I) -> Atom I
    (Atom F) -> Atom F
    (Not p) -> not (evalK3 p)
    (And p q) -> and (evalK3 p) (evalK3 q)
    (Or p q) -> or (evalK3 p) (evalK3 q)
    (Imp p q) -> imp (evalK3 p) (evalK3 q)
    (Iff p q) -> iff (evalK3 p) (evalK3 q)
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
        I -> Atom I
        F -> Atom I
    F -> Atom T
imp _ _ = undefined

iff :: Formula V -> Formula V -> Formula V
iff ap@(Atom _) aq@(Atom _) = imp ap aq `and` imp aq ap
iff _ _ = undefined

-- | Semantics of K3
--
semantics :: Semantics V
semantics = makeSemantics trvK3 evalK3
