module Parser where

import Control.Applicative
import Data.Map (Map)
import Data.Text (Text)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text
import qualified Data.Map as Map
import qualified Data.Text as Text

type WordList = Map [Text] Integer

wordList :: Parser WordList
wordList = Map.fromList <$> line `sepBy` ((return <$> char '\n') <|> string ", ")

line :: Parser ([Text], Integer)
line = (,) <$> word `sepEndBy1` spaces <*> integer

word :: Parser Text
word = Text.pack <$> many1 letter

integer :: Parser Integer
integer = read <$> (positive <|> negative <|> number)
  where positive = char '+' *> number
        negative = (:) <$> char '-' <*> number
        number = many digit
