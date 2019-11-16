module Uwecode.Parser.CharsAndStrings where

import Data.Map

arrowStr = "->"
equalsStr = "="
spaceChars = [' ', '\t', '\r', '\n']
numChars = ['0'..'9']
alphaChars = ['a'..'z'] ++ ['A'..'Z']
normalSymbolChars = "~!@#$%^&*_+-\\|:;<>?,./"
wordChars = numChars ++ alphaChars ++ normalSymbolChars
backtick = '`'
singleQuote = '\''
doubleQuote = '"'
quotes = "'\""
enclosers = fromList [('(', ')'), ('[', ']'), ('{', '}')]
