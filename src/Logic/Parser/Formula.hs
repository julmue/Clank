{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Parser.Formula
--

module Logic.Parser.Formula
    (
    -- * Parsers
      formula
    ) where

import Data.Functor.Identity (Identity)

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as Expr

import qualified Logic.Data.Formula as F

{- Lexer -}

-- parameterization of the lexer
languageDef :: Token.LanguageDef ()
languageDef = Token.LanguageDef
    {   Token.commentStart      = "/*"
    ,   Token.commentEnd        = "*/"
    ,   Token.commentLine       = "//"
    ,   Token.nestedComments    = False
    ,   Token.identStart        = letter
    ,   Token.identLetter       = alphaNum <|> oneOf "_'"
    ,   Token.opStart           = Token.opLetter languageDef
    ,   Token.opLetter          = oneOf "`~!@$%^&+-*/=;:<>.?"
    ,   Token.reservedOpNames   = [".","~","&&", "||","->","<->","forall","exists"]
    ,   Token.reservedNames     = ["false","true","not","and","or","imp","iff","forall","exists"]
    ,   Token.caseSensitive     = True
    }

-- construction of the lexer using the lexerStyle parameter-collection
-- () means no user definede state
-- lexer is a collection of lexival parsers
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

-- extract the lexical parsers from 'lexer'
-- these parsers strip trainling whitespace after tokens
identifier  :: Parser String
identifier  = Token.identifier lexer
reserved    :: String -> Parser ()
reserved    = Token.reserved lexer
reservedOp  :: String -> Parser ()
reservedOp  = Token.reservedOp lexer
parens      :: Parser a -> Parser a
parens      = Token.parens lexer
-- integer     :: Parser Integer
-- integer     = Token.integer lexer
-- semi        :: Parser String
-- semi        = Token.semi lexer
whiteSpace  :: Parser ()
whiteSpace  = Token.whiteSpace lexer

{- main parser -}
true :: Parser (F.Formula a)
true =
    reserved "true" >>
    whiteSpace >>
    return F.True

false :: Parser (F.Formula a)
false =
    reserved "false" >>
    whiteSpace >>
    return F.False

atom :: Parser a -> Parser (F.Formula a)
atom p =
    p >>= \content ->
    whiteSpace >>
    return (F.Atom content)

operators :: [[Expr.Operator String () Identity (F.Formula a)]]
operators =
    [   [Expr.Prefix (reservedOp "forall"   >> identifier >>= \i -> reservedOp "." >> return (F.Forall i))
        ,Expr.Prefix (reservedOp "exists"   >> identifier >>= \i -> reservedOp "." >> return (F.Exists i))   ]
    ,   [Expr.Prefix (reservedOp "~"        >> return F.Not )                                                ]
    ,   [Expr.Infix  (reservedOp "&&"       >> return F.And )                       Expr.AssocRight          ]
    ,   [Expr.Infix  (reservedOp "||"       >> return F.Or  )                       Expr.AssocRight          ]
    ,   [Expr.Infix  (reservedOp "->"       >> return F.Imp )                       Expr.AssocRight          ]
    ,   [Expr.Infix  (reservedOp "<->"      >> return F.Iff )                       Expr.AssocRight          ]
    ]

-- | (Parsec) Parser for 'Formula'
-- Takes another Parser for the payload of the 'Formula' as an argument.
--
formula :: Parser a -> Parser (F.Formula a)
formula p =
    Expr.buildExpressionParser operators
    (    parens (formula p)
     <|> true
     <|> false
     <|> atom p
    )
