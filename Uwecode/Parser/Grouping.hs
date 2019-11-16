module Uwecode.Parser.Grouping where

import Uwecode.Parser.Parser
import Uwecode.Parser.CharsAndStrings
import Data.Map

data QuotedState   = QuotedState { isStr :: Bool, isBackticked :: Bool }
data EnclosedState = EnclosedState { endingChar :: Char }

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

enclosedParser :: EnclosedState -> Parser String
enclosedParser state = do
    c <- singleChar
    if elem c quotes
        then do
            s <- quotedParser $ QuotedState (c == doubleQuote) False
            s2 <- enclosedParser state
            return ((c:s) ++ (c:s2))
    else if member c enclosers
        then do
            s <- enclosedParser $ EnclosedState $ enclosers ! c
            s2 <- enclosedParser state
            return ((c:s) ++ (enclosers ! c):s2)
    else if c == (endingChar state)
        then return ""
    else do
        s2 <- enclosedParser state
        return (c:s2)
