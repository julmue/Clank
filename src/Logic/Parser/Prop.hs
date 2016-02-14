{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Parser.Prop
--

module Logic.Parser.Prop
    ( prop
    , formulaProp
    ) where

import Text.Parsec.Char (letter)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)

import Logic.Parser.Formula
import Logic.Data.Prop
import Logic.Data.Formula

-- | (Parsec) Parser for 'Prop'
-- Parser for 'Prop'.
--
prop :: Parser Prop
prop =  Prop <$> many1 letter

-- | (Parsec) Parser for 'Formula' 'Prop'
-- Parser for 'Formula' 'Prop'.
--
formulaProp :: Parser (Formula Prop)
formulaProp = formula prop
