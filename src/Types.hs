module Types where

import qualified Data.Text                  as T
import           Data.Text.Prettyprint.Doc
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Name = Name T.Text deriving (Show, Eq, Ord, Pretty)

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parseName :: Parser Name
parseName = Name . T.pack <$> lexeme (some alphaNumChar)
