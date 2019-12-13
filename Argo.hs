{-# LANGUAGE LambdaCase #-}

module Argo where

import Control.Applicative
import Data.Char (isDigit, isSpace)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a
  = Parser
      { runParser :: String -> Maybe (String, a)
      }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \x -> do
    (x', y) <- p x
    Just (x', f y)

instance Applicative Parser where

  pure x = Parser $ \y -> Just (y, x)

  Parser p1 <*> Parser p2 = Parser $ \x -> do
    (x', f) <- p1 x
    (x'', a) <- p2 x'
    Just (x'', f a)

instance Alternative Parser where

  empty = Parser $ const Nothing

  Parser f <|> Parser g = Parser $ \x -> f x <|> g x

-- Parser Combinators

char :: Char -> Parser Char
char c = Parser $ \case
  x : xs | x == c -> Just (xs, c)
  _ -> Nothing

string :: String -> Parser String
string = traverse char

partition :: (Char -> Bool) -> Parser String
partition f = Parser $ \x ->
  let (token, rest) = span f x
   in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \x -> do
  (x', xs) <- p x
  if null xs then Nothing else Just (x', xs)

-- Parsers

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser JsonValue
jsonBool = JsonBool . ("true" ==) <$> (string "true" <|> string "false")

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNull (partition isDigit)

stringLiteral :: Parser String
stringLiteral = char '"' *> partition (/= '"') <* char '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = partition isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep el = (:) <$> el <*> many (sep *> el) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (char '[' *> ws *> sepBy (ws *> char ',' <* ws) jsonValue <* ws <* char ']')

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (char '{' *> ws *> sepBy (ws *> char ',' <* ws) keyValue <* ws <* char '}')
  where
    keyValue = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> char ':' <* ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile file parser = do
  input <- readFile file
  pure $ snd <$> runParser parser input
