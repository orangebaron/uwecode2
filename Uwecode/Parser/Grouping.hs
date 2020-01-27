module Uwecode.Parser.Grouping where

import Uwecode.Parser.Parser
import Uwecode.Parser.CharsAndStrings
import Data.Map
import Numeric.Natural
import Control.Applicative

data QuotedState   = QuotedState { isStr :: Bool, isBackticked :: Bool }

isLastQuoteChr :: QuotedState -> Char -> Bool
isLastQuoteChr state c
    | isBackticked state = False
    | isStr state        = c == doubleQuote
    | otherwise          = c == singleQuote

transformQuoteState :: QuotedState -> Char -> QuotedState
transformQuoteState state c = QuotedState (isStr state) newIsBackticked where
    newIsBackticked = (c == '\\' && not (isBackticked state))

quotedParser :: QuotedState -> Parser String
quotedParser state = do
    c <- singleChar
    if (isLastQuoteChr state c) then return "" else do
        s <- quotedParser $ transformQuoteState state c
        return (c:s)

enclosedParser :: Char -> Parser String
enclosedParser endingChar = do
    c <- singleChar
    if elem c quotes
        then do
            s <- quotedParser $ QuotedState (c == doubleQuote) False
            s2 <- enclosedParser endingChar
            return ((c:s) ++ (c:s2))
    else if member c enclosers
        then do
            s <- enclosedParser (enclosers ! c)
            s2 <- enclosedParser endingChar
            return ((c:s) ++ (enclosers ! c):s2)
    else if c == endingChar
        then return ""
    else do
        s2 <- enclosedParser endingChar
        return (c:s2)

comment :: Parser String
comment = specificChar '[' >> enclosedParser ']'

spaceWithComments :: Parser String
spaceWithComments = listed (space >> comment) >> space

afterToken :: Parser String
afterToken = spaceWithComments <|> wantEmpty ""

token :: Parser a -> Parser a
token p = do
    a <- p
    afterToken
    return a

stringToken :: String -> Parser String
stringToken = token . specificString

word :: Parser String
word = token $ oneOrMoreIn wordChars

number :: Parser Natural
number = token $ do
    numStr <- oneOrMoreIn numChars
    return $ read numStr

backtickPrefacedWord = specificChar backtick >> word
