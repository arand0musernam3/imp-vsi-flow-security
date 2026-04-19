module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

import Imp

-- Lexer definition
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentStart    = "/*"
      , Token.commentEnd      = "*/"
      , Token.commentLine     = "//"
      , Token.reservedNames   = ["skip", "if", "then", "else", "while", "do", "stop",
                                 "input", "output"]
      , Token.reservedOpNames = [":=", "+", "-", "*", ";", ","]
      }

integer    = Token.integer lexer
identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
braces     = Token.braces lexer
semi       = Token.semi lexer
comma      = Token.comma lexer
whiteSpace = Token.whiteSpace lexer

-- Level Parser
-- We allow any identifier to be a level, but we specifically match common ones
level :: Parser Level
level = L <$> identifier

-- Expression Parser
expression :: Parser Expr
expression = buildExpressionParser operators term
  where
    operators = [ [Infix (reservedOp "*" >> return (BinOpExpr Times)) AssocLeft]
                , [Infix (reservedOp "+" >> return (BinOpExpr Plus))  AssocLeft,
                   Infix (reservedOp "-" >> return (BinOpExpr Minus)) AssocLeft]
                ]
    term =  parens expression
        <|> (IntExpr <$> integer)
        <|> (VarExpr <$> identifier)

-- Command Parser
command :: Parser Cmd
command = semiSep1 statement >>= return . foldl1 Seq

statement :: Parser Cmd
statement =  (reserved "skip" >> return Skip)
         <|> (reserved "stop" >> return Stop)
         <|> (reserved "input" >> parens (do
                l <- level
                comma
                Input l <$> identifier))
         <|> (reserved "output" >> parens (do
                l <- level
                comma
                Output l <$> expression))
         <|> (reserved "if" >> do
                cond <- expression
                reserved "then"
                c1 <- commandBlock
                reserved "else"
                If cond c1 <$> commandBlock)
         <|> (reserved "while" >> do
                cond <- expression
                reserved "do"
                While cond <$> commandBlock)
         <|> (do
                var <- identifier
                reservedOp ":="
                Assign var <$> expression)

commandBlock :: Parser Cmd
commandBlock = braces command <|> parens command <|> statement

semiSep1 :: Parser a -> Parser [a]
semiSep1 p = p `sepBy1` reservedOp ";"

-- Main Parser
parseImp :: String -> Either ParseError Cmd
parseImp = parse (whiteSpace >> command <* eof) ""
