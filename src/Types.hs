module Types where

import           Data.Text.Prettyprint.Doc
import qualified Data.Text                 as T

newtype Name = Name T.Text deriving (Show, Eq, Ord, Pretty)
