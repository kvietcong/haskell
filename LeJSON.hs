import Data.Char (isSpace, toLower, digitToInt, isDigit)
import Data.List (intercalate)
import Control.Applicative
import Data.Bifunctor
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Tuple
import System.IO
import Text.Read

{-
   This is an attempt to make a JSON Parser. I thought this would
   be fairly straightforward at first but I soon realized I would
   need to understand a lot more about Haskell to parse JSON in
   an effective manner.

   I have learned a lot so far and still haven't finished lol.
   From Functors to Monads, this has been a really enlightening
   experience.

   Tutorials I have used so far:
   - https://github.com/tsoding/haskell-json
   - https://wiki.haskell.org/Typeclassopedia
   - https://youtube.com/playlist?list=PLfzJKXh_D71QzOEhX0eWm7XChn_C5Rrxo
   - https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/#parser
-}

surround' :: String -> String -> String -> String
surround' pre post mid = pre ++ mid ++ post

surround :: String -> String -> String
surround prepost mid = prepost ++ mid ++ prepost

surroundQuote :: String -> String
surroundQuote = surround "\""

surroundBracket :: String -> String
surroundBracket = surround' "[" "]"

surroundBrace :: String -> String
surroundBrace = surround' "{" "}"

-- |A type to represent JSON Values
data JValue = JObject [(String, JValue)]
            | JArray [JValue]
            | JString String
            | JNumber Double
            | JBool Bool
            | JNull
            deriving Eq

instance Show JValue where
    show jVal = case jVal of
        JObject objects     -> showObjects objects
        JArray jValues      -> surroundBracket . intercalate ", " $ map show jValues
        JString string      -> surroundQuote string
        JNumber number      -> show number
        JBool bool          -> map toLower . show $ bool
        JNull               -> "null"
        where showObjects   = surroundBrace
                                . intercalate ", "
                                . map (\(key, jValue) ->
                                       surroundQuote key ++ ": " ++ show jValue)

-- |A parser for some type `a`
newtype Parser a = Parser { runParser :: String  -> Maybe (String, a) }

instance Functor Parser where
    -- We need to get past the Maybe type then map the function onto the parsed value
    fmap function parser = Parser $ fmap (second function) . runParser parser

instance Applicative Parser where
    pure x = Parser $ \toParse -> Just (toParse, x)
    (Parser parser) <*> (Parser parser') =
        Parser $ \toParse -> do
            (toParse', parsed) <- parser toParse
            (toParse'', parsed') <- parser' toParse'
            pure (toParse'', parsed parsed')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser parser) <|> (Parser parser') =
        Parser $ \toParse -> parser toParse <|> parser' toParse

-- `function` returns a new parser based on what is outputted from `parser`
instance Monad Parser where
    parser >>= getNewParser = Parser $ \toParse->
        case runParser parser toParse of
          Just (toParse', parsed) ->
              runParser (getNewParser parsed) toParse'
          Nothing -> Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser predicate = Parser $
    \chars -> case chars of
                (x:xs) -> if predicate x
                             then Just (xs, x)
                             else Nothing
                []     -> Nothing

inBetweenParser :: Parser a -> Parser b -> Parser c -> Parser a
inBetweenParser betweenParser openParser closeParser =
    openParser *> betweenParser <* closeParser

surroundParser :: Parser a -> Parser b -> Parser a
surroundParser surroundedParser surrounderParser =
    inBetweenParser surroundedParser surrounderParser surrounderParser

charParser :: Char -> Parser Char
charParser c =  predicateParser (==c)

anyCharParser :: Parser Char
anyCharParser =  predicateParser (const True)

takeWhileParser :: (Char -> Bool) -> Parser String
takeWhileParser = many . predicateParser

whitespaceParser :: Parser String
whitespaceParser = takeWhileParser isSpace

stringParser :: String -> Parser String
stringParser = traverse charParser

seperatedByParser :: Parser a -> Parser b -> Parser [a]
seperatedByParser parser delimiter = do
    maybeElement <- optional parser
    maybeDelimited <- optional delimiter
    case (maybeElement, maybeDelimited) of
      (Just element, Just _) -> do
          elements <- seperatedByParser parser delimiter
          pure (element:elements)
      (Just element, Nothing)   -> pure [element]
      _ -> empty

-- Only supports escaped sequences of one character length (No unicode yet)
jCharParser :: Parser Char
jCharParser = (charParser '\\' *> anyCharParser) <|> predicateParser (/='"')

jNullParser :: Parser JValue
jNullParser = JNull <$ stringParser "null"

jBoolParser :: Parser JValue
jBoolParser = JBool True <$ stringParser "true" <|> JBool False <$ stringParser "false"

{-
-- This will throw an exception. Not wanted
jNumParser :: Parser JValue
jNumParser = readJNum <$> parseValidNumChars
    where isValidNumChar = flip elem (['0'..'9'] ++ ['-', 'e', '.'])
          parseValidNumChars = takeWhileParser isValidNumChar
          readJNum num = JNumber $ read num
-- I want to find a way to compose parsers to parser numbers but IDK how to combine
-- parser results unless I use do notation
jNumParser = JNumber . read <$> validNumParser
    where validNumParser = optional (charParser '-')
            *> some (predicateParser isDigit)
            <* optional (charParser 'e' <|> charParser 'E')
            <* some (predicateParser isDigit)
-}

-- This way seems wrong (probably is lol)
jNumParser :: Parser JValue
jNumParser = Parser $ \input -> do
    let isValidNumChar = flip elem (['0'..'9'] ++ ['-', 'e', '.'])
    (rest, toRead) <- runParser (takeWhileParser isValidNumChar) input
    case readMaybe toRead of
        Just number -> pure (rest, JNumber number)
        _           -> Nothing

jStringParser :: Parser JValue
jStringParser = JString <$> surroundParser (many jCharParser) (charParser '"')

jArrayParser :: Parser JValue
jArrayParser = JArray <$>
    inBetweenParser (elementsParser <|> ([] <$ whitespaceParser)) (charParser '[') (charParser ']')
        where elementsParser = seperatedByParser
                               (whitespaceParser *> jValueParser <* whitespaceParser)
                               (charParser ',')

jObjectParser :: Parser JValue
jObjectParser = JObject <$>
    inBetweenParser (elementsParser <|> ([] <$ whitespaceParser)) (charParser '{') (charParser '}')
        where elementParser = do
                  key <- surroundParser (many jCharParser) (charParser '"')
                  _ <- whitespaceParser *> charParser ':' <* whitespaceParser
                  value <- jValueParser
                  pure (key, value)
              elementsParser = seperatedByParser 
                               (whitespaceParser *> elementParser <* whitespaceParser)
                               (charParser ',')

jValueParser :: Parser JValue
jValueParser = jNullParser <|> jBoolParser <|> jNumParser <|> jStringParser <|> jArrayParser <|> jObjectParser

parseJSON :: String -> Maybe (String, JValue)
parseJSON = runParser jValueParser

parseJSONFile :: String -> IO (Maybe (String, JValue))
parseJSONFile path = parseJSON <$> readFile path

main :: IO ()
main = do
    parsedValue <- parseJSONFile "testJSON.json"
    print parsedValue
    pure ()
