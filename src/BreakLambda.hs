{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module BreakLambda where

import qualified Lambda                    as Lam
import           Types

import           Data.Functor.Foldable
import           Data.Functor.Sum

import           Data.Text.Prettyprint.Doc

import           Text.Megaparsec

data BreakF x = BreakComputeF x

type Exp = Fix (Sum Lam.ExpF BreakF)

{-# COMPLETE Var, Lambda, Apply, BreakCompute #-}
pattern Var :: Name -> Exp
pattern Var n = Fix (InL (Lam.VarF n))
pattern Lambda :: Name -> Exp -> Exp
pattern Lambda n body = Fix (InL (Lam.LambdaF n body))
pattern Apply :: Exp -> Exp -> Exp
pattern Apply t1 t2 = Fix (InL (Lam.ApplyF t1 t2))
pattern BreakCompute :: Exp -> Exp
pattern BreakCompute e = Fix (InR (BreakComputeF e))

instance Pretty Exp where
  pretty = \case
    Var n -> pretty n
    Lambda n body -> parens ("\\" <> pretty n <+> "." <+> pretty body)
    Apply t1 t2 -> parens (pretty t1 <> pretty t2)
    BreakCompute t -> "*" <> pretty t

parseExp :: Parser Exp
parseExp = parseVar <|> parseApp <|> parseLam <|> parsePar <|> parseBreak

parseVar :: Parser Exp
parseVar = Var <$> parseName

parseApp :: Parser Exp
parseApp = Apply <$> parsePar <*> parsePar

parseLam :: Parser Exp
parseLam = Lambda <$> (symbol "\\" *> parseName <* symbol ".") <*> parseExp

parseBreak :: Parser Exp
parseBreak = BreakCompute <$> (symbol "*" *> parseExp)

parsePar :: Parser Exp
parsePar = between (symbol "(") (symbol ")") parseExp
