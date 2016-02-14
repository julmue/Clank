{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Semantics.Prop
--
-- External interface of the "Logic.Semantics.Prop".
--

module Logic.Semantics.Prop
    (
    -- * Types
      Semantics (..)
    , TrVals (..)
    , Property(..)
    -- * Constructor functions
    , makeTrVals
    , makeSemantics
    -- * Prototype functions
    , protoDomain
    , protoModels
    , protoModelsLookup
    )
where

import Logic.Semantics.Prop.Internal
