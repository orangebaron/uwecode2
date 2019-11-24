module Uwecode.Parser.Parser where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Uwecode.Parser.CharsAndStrings
import Control.Monad
import Control.Applicative
import Numeric.Natural

data Parser a = Parser { parse :: String -> [(a, String)] }

instance Monad Parser where
    return a = Parser (\s -> [(a, s)])
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
    empty = Parser $ const []
    p1 <|> p2 = Parser (\s -> parse p1 s <|> parse p2 s)

instance MonadPlus Parser

takeFirstParse :: Parser a -> String -> Maybe a
takeFirstParse parser str = case (filter ((== "") . snd) $ parser `parse` str) of
    []        -> Nothing
    ((a,_):_) -> Just a

returnList :: [a] -> Parser a
returnList l = Parser (\s -> map (\x -> (x, s)) l)

returnGoodFromParse :: Parser a -> String -> Parser a
returnGoodFromParse parser str = returnList . map fst . filter (\x -> snd x == "") $ parser `parse` str

maybeReturn :: Maybe a -> Parser a
maybeReturn = maybe empty return

singleChar :: Parser Char
singleChar = Parser (\s -> case s of -- TODO: use case more in other stuff :)
    ""     -> []
    (c:s2) -> [(c, s2)])

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
    a  <- p
    as <- listed p
    return (a:as)

separatedBy :: Parser a -> Parser b -> Parser c -> (a -> c -> d) -> Parser d
separatedBy p1 p2 p3 f = do
    a <- p1
    p2
    c <- p3
    return $ f a c

zeroOrMoreIn :: [Char] -> Parser String
zeroOrMoreIn cs = listed $ charSatisfies (`elem` cs)

oneOrMoreIn :: [Char] -> Parser String
oneOrMoreIn cs = oneOrMoreListed $ charSatisfies (`elem` cs)

space :: Parser String
space = oneOrMoreIn spaceChars

nonSpaces :: Parser String
nonSpaces = oneOrMoreListed $ charSatisfies $ not . (`elem` spaceChars)

wantEmpty :: a -> Parser a
wantEmpty a = Parser (\s -> case s of
    "" -> [(a, "")]
    _  -> [])

afterToken :: Parser String
afterToken = space <|> wantEmpty ""

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

stringsEndingInSpaceShortFirst :: Parser String
stringsEndingInSpaceShortFirst = do
    s1 <- nonSpaces
    s2 <- space
    return (s1 ++ s2) <|> do
        s3 <- stringsEndingInSpaceShortFirst
        return (s1 ++ s2 ++ s3)

stringsEndingInSpaceLongFirst :: Parser String
stringsEndingInSpaceLongFirst = Parser $ reverse . parse stringsEndingInSpaceShortFirst
