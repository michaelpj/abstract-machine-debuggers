{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lambda where

import           Types

import           Data.Functor.Foldable

import           Data.Text.Prettyprint.Doc

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

aTerm :: Exp
aTerm = Lambda (Name "x") (Var (Name "x")) `Apply` Lambda (Name "y") (Var (Name "y"))

bTerm :: Exp
bTerm = Lambda (Name "y") (Lambda (Name "x") (Var (Name "x")) `Apply` Var (Name "y")) `Apply` (Lambda (Name "u") (Var (Name "u")) `Apply` Lambda (Name "y") (Var (Name "y")))

iTerm :: Exp
iTerm = Lambda (Name "i") (Var (Name "i"))

kTerm :: Exp
kTerm = Lambda (Name "a") (Lambda (Name "b") (Var (Name "a")))

loop :: Exp
loop = Lambda (Name "x") (Var (Name "x") `Apply` Var (Name "x")) `Apply` Lambda (Name "x") (Var (Name "x") `Apply` Var (Name "x"))

cbvVsCbn :: Exp
cbvVsCbn = kTerm `Apply` loop `Apply` iTerm
