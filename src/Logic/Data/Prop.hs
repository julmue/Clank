{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Data.Formula
--
-- External interface of "Logic.Data.Prop".
--

module Logic.Data.Prop
    (
    -- * Types
    Prop(..)
    ) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- | Type of propositions
--
newtype Prop = Prop { propName :: String } deriving (Eq, Ord)

-- | 'Prop' instance of Show.
instance Show Prop where
    show = propName

instance Arbitrary Prop where
    arbitrary = (elements . fmap Prop) ["A","B","C","D"]

