module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser mapper where
    mapper input = case runParser p input of
      Just (result, remaining) -> Just (f result, remaining)
      Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser (\input -> Just (x, input))
  ff <*> fa = Parser go where
    go input = do
          (f, remaining) <- runParser ff input
          (result, remaining1) <- runParser fa remaining
          return (f result, remaining1)

instance Monad Parser where
  return = pure
  fa >>= f = Parser go where
    go input = do
      (result, remaining) <- runParser fa input
      runParser (f result) remaining

instance Alternative Parser where
  empty = Parser (\x -> Nothing)
  p1 <|> p2 = Parser go where
    go input = case runParser p1 input of
      Nothing -> runParser p2 input
      result -> result

conditional p = Parser (go) where
    go input = case input of
      (x:xy) | (p x) -> Just (x, xy)
      otherwise -> Nothing

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep p = (:) <$> p <*> (many $ sep *> p)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = sepBy1 sep p <|> pure []

char c = conditional (==c)

whitespace = conditional isSpace

whitespaces = many whitespace

whitespaces1 = some whitespace

alphaNum = alphaNum1 <|> pure []

alphaNum1 = (:) <$> (conditional isAlpha) <*> (many $ conditional isAlphaNum)


fraction = (:) <$> char '.' <*> fmap show digits
floatingNumber = ((++) <$> fmap show digits <*> fraction)

float :: Parser Float
float = fmap read floatingNumber

digits :: Parser Int
digits = fmap read . some $ conditional isDigit

string :: String -> Parser ()
string = void . sequence . (fmap char)

newLine =  void $ char '\n'

between left center right = left *> center <* right

someFunc :: IO ()
someFunc = do
  configFile <- readFile "testConfig.txt"
  putStrLn configFile
  print $ runParser config configFile


data ConfigValue = IntValue Int | StringValue String | FloatValue Float deriving Show

newtype SectionName = SectionName String deriving Show
data Assignment = Assignment String ConfigValue deriving Show
data Section = Section SectionName [Assignment] deriving Show

configValue :: Parser ConfigValue
configValue = FloatValue <$> float <|> IntValue <$> digits <|> StringValue <$> alphaNum1

sectionName = SectionName <$> between (string "[[") (whitespaces1 *> alphaNum1 <* whitespaces1) (string "]]")

assignment = Assignment <$> alphaNum1 <* whitespace <* char '=' <* whitespace <*> configValue

section =  do
  many newLine
  name <- sectionName
  newLine
  assignments <- sepBy1 (some newLine) assignment
  return $ Section name assignments

config = sepBy1 (some newLine) section

