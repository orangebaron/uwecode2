module Uwecode.Parser.CharsAndStrings where

import Data.Map

fileExtension = ".uwe"
importPrefixStr = "+"
importSpecificStr = ":"
arrowStr = "->"
equalsStr = "="
privateEqualsStr = "_="
keywords = [arrowStr, equalsStr, privateEqualsStr]
spaceChars = [' ', '\t', '\r', '\n']
numChars = ['0'..'9']
alphaChars = ['a'..'z'] ++ ['A'..'Z']
normalSymbolChars = "~!@#$%^&*_+-\\|:;<>?,./="
wordChars = numChars ++ alphaChars ++ normalSymbolChars
backtick = '`'
singleQuote = '\''
doubleQuote = '"'
quotes = "'\""
enclosers = fromList [('(', ')'), ('[', ']'), ('{', '}')]
backslashReplacements = fromList [('n', '\n'), ('r', '\r'), ('t', '\t')]
