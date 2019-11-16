module Uwecode.Parser.Parser where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Uwecode.Parser.CharsAndStrings
import Control.Monad
import Control.Applicative
import Numeric.Natural

data Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Monad Parser where
    return a = Parser (\s -> Just (a, s))
    p >>= f  = Parser (\s -> do
        (a, s2) <- parse p s
        parse (f a) s2)

instance Functor Parser where
    fmap f p = Parser (\s -> fmap (\(a, s2) -> (f a, s2)) $ parse p s)

instance Applicative Parser where
    pure = return
    a <*> b = Parser (\s -> do
        (f, s2) <- parse a s
        parse (fmap f b) s2)

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser (\s -> parse p1 s <|> parse p2 s)

instance MonadPlus Parser

singleChar :: Parser Char
singleChar = Parser (\s -> case s of -- TODO: use case more in other stuff :)
    "" -> Nothing
    (c:s2) -> Just (c, s2))

charSatisfies :: (Char -> Bool) -> Parser Char
charSatisfies f = do
    c <- singleChar
    guard $ f c
    return c

specificChar :: Char -> Parser Char
specificChar = charSatisfies . (==)

specificString :: String -> Parser String
specificString "" = return ""
specificString (c:s) = do
    specificChar c
    specificString s
    return (c:s)

listed :: Parser a -> Parser [a]
listed p = oneOrMoreListed p <|> return []

oneOrMoreListed :: Parser a -> Parser [a]
oneOrMoreListed p = do
    a <- p
    as <- listed p
    return (a:as)

separatedBy :: Parser a -> Parser b -> Parser c -> (a -> c -> d) -> Parser d
separatedBy p1 p2 p3 f = do
    a <- p1
    p2
    c <- p3
    return $ f a c

oneOrMoreIn :: [Char] -> Parser String
oneOrMoreIn cs = oneOrMoreListed $ charSatisfies (`elem` cs)

space :: Parser String
space = oneOrMoreIn spaceChars

wantEmpty :: Parser ()
wantEmpty = Parser (\s -> case s of
    "" -> Just ((), "")
    _  -> Nothing)

afterToken :: Parser ()
afterToken = (space >> return ()) <|> wantEmpty

token :: Parser a -> Parser a
token p = do
    a <- p
    afterToken
    return a

stringToken :: String -> Parser String
stringToken = token . specificString

arrowToken = stringToken arrowStr
equalsToken = stringToken equalsStr

word :: Parser String
word = token $ oneOrMoreIn wordChars

number :: Parser Natural
number = token $ do
    numStr <- oneOrMoreIn numChars
    return $ read numStr

backtickPrefacedWord = specificChar backtick >> word
