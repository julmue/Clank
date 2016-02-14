{-# OPTIONS_GHC -Wall -Werror #-}

module Logic.Parser.Parser
    ( Parser
    , parseString
    ) where

import Text.Parsec.Char (spaces)
import Text.Parsec.String (Parser)

-- Strips trailing whitespace and parses String
parseString :: Parser a -> Parser a
parseString p =
    spaces >>
    p

