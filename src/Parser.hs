module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity
import Data.List (nub)

import Imp
import Types (mkSecurityLattice, stdLatticeNames, stdLatticeRelations)

-- Use SecurityLattice as state
type LParser a = Parsec String SecurityLattice a

-- Lexer definition
lexer :: Token.GenTokenParser String SecurityLattice Identity
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentStart    = "/*"
      , Token.commentEnd      = "*/"
      , Token.commentLine     = "//"
      , Token.reservedNames   = ["skip", "if", "then", "else", "while", "do", "stop",
                                 "input", "output", "def", "return", "call", "lattice", "erase"]
      , Token.reservedOpNames = [":=", "+", "-", "*", ";", ",", "<"]
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
level :: LParser Level
level = do
    name <- identifier
    lat <- getState
    case filter (\l -> lName l == name) (latticeLevels lat) of
        (l:_) -> return l
        []    -> fail $ "Level " ++ name ++ " not found in lattice"

-- Expression Parser
expression :: LParser Expr
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
command :: LParser Cmd
command = do
    cmds <- statement `sepBy1` reservedOp ";"
    optional (reservedOp ";")
    return $ foldl1 Seq cmds

statement :: LParser Cmd
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
         <|> (reserved "erase" >> parens (do
                l <- level
                comma
                Erase l <$> identifier))
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
                let callP = do
                      reserved "call"
                      fName <- identifier
                      args <- parens (expression `sepBy` comma)
                      return $ Call var fName args
                    assignP = Assign var <$> expression
                callP <|> assignP)

commandBlock :: LParser Cmd
commandBlock = braces command <|> parens command <|> statement

semiSep1 :: LParser a -> LParser [a]
semiSep1 p = p `sepBy1` reservedOp ";"

-- Function Parser
functionDef :: LParser Function
functionDef = do
    reserved "def"
    fName <- identifier
    args <- parens (identifier `sepBy` comma)
    body <- braces command
    reserved "return"
    retExpr <- expression
    return $ Function fName args body retExpr

-- Lattice Definition Parser
-- Supports both:
-- 1. lattice { a, b, c } -> interpreted as a < b < c
-- 2. lattice { a < b, a < c, b < d, c < d } -> arbitrary partial order
latticeDef :: LParser ()
latticeDef = (do
    reserved "lattice"
    braces $ do
      -- Try to parse as relations first
      rels <- (do
          r <- try (relation `sepBy` comma)
          if null r then fail "empty" else return (Left r)
        ) <|> (do
          names <- identifier `sepBy` comma
          return (Right names)
        )
      case rels of
        Left r -> do
          let names = nub $ concat [ [u, v] | (u, v) <- r ]
          putState $ mkSecurityLattice names r
        Right names -> do
          let r = zip names (tail names)
          putState $ mkSecurityLattice names r
  ) <|> return ()
  where
    relation = do
      u <- identifier
      reservedOp "<"
      v <- identifier
      return (u, v)

-- Program Parser
program :: LParser Program
program = do
    whiteSpace
    latticeDef
    optional (reservedOp ";")
    fns <- many (do { f <- functionDef; optional (reservedOp ";"); return f })
    mainCmd <- command
    lat <- getState
    return $ Program lat fns mainCmd

-- Main Parser
parseImp :: String -> Either ParseError Program
parseImp = runParser (whiteSpace >> program <* eof) (mkSecurityLattice stdLatticeNames stdLatticeRelations) ""
