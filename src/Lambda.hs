{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Untyped lambda calculus.
module Lambda where

import           Types

import           Data.Functor.Foldable

import           Data.Text.Prettyprint.Doc

import           Text.Megaparsec

data ExpF x = VarF Name
            | LambdaF Name x
            | ApplyF x x
             deriving (Functor, Traversable, Foldable)

type Exp = Fix ExpF

{-# COMPLETE Var, Lambda, Apply #-}
pattern Var :: Name -> Exp
pattern Var n = Fix (VarF n)
pattern Lambda :: Name -> Exp -> Exp
pattern Lambda n body = Fix (LambdaF n body)
pattern Apply :: Exp -> Exp -> Exp
pattern Apply t1 t2 = Fix (ApplyF t1 t2)

instance Pretty Exp where
  pretty = \case
    Var n -> pretty n
    Lambda n body -> parens ("\\" <> pretty n <+> "." <+> pretty body)
    Apply t1 t2 -> parens (pretty t1 <> pretty t2)

parseExp :: Parser Exp
parseExp = parseVar <|> parseApp <|> parseLam <|> parsePar

parseVar :: Parser Exp
parseVar = Var <$> parseName

parseApp :: Parser Exp
parseApp = Apply <$> parsePar <*> parsePar

parseLam :: Parser Exp
parseLam = Lambda <$> (symbol "\\" *> parseName <* symbol ".") <*> parseExp

parsePar :: Parser Exp
parsePar = between (symbol "(") (symbol ")") parseExp
