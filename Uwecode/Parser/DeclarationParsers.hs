module Uwecode.Parser.DeclarationParsers where

import Uwecode.Parser.Parser
import Uwecode.Parser.CharsAndStrings
import Uwecode.Parser.ExpressionParsers
import Uwecode.Parser.Grouping
import Uwecode.AST
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad

equalsDeclaration :: Parser DeclarationAST
equalsDeclaration = separatedBy word equalsToken expression Equals

appendToFirst :: a -> [a] -> [[a]] -> [[a]]
appendToFirst a b (c:d) = ((a:(b++c)):d)
appendToFirst a b []  = [a:b]

checkForStartOfEqualsDecl :: Char -> Parser [String]
checkForStartOfEqualsDecl c = do
    s1 <- zeroOrMoreIn wordChars
    s2 <- space
    s3 <- specificString equalsStr
    s4 <- space
    decls <- separateDeclarationsHelper
    return $ "":(appendToFirst c (s1++s2++s3++s4) decls)

readPossiblyEmptyWordAndContinue :: Char -> Parser [String]
readPossiblyEmptyWordAndContinue c = do
    s <- zeroOrMoreIn wordChars
    decls <- separateDeclarationsHelper
    return $ appendToFirst c s decls

removeFirstIfEmpty :: [String] -> [String]
removeFirstIfEmpty ("":rest) = rest
removeFirstIfEmpty x = x

separateDeclarationsHelper :: Parser [String]
separateDeclarationsHelper = wantEmpty [] <|> do
    c <- singleChar
    if elem c quotes
        then do
            s <- quotedParser $ QuotedState (c == doubleQuote) False
            decls <- separateDeclarationsHelper
            return $ appendToFirst c (s++[c]) decls
    else if Map.member c enclosers
        then do
            s <- enclosedParser $ EnclosedState $ enclosers Map.! c
            decls <- separateDeclarationsHelper
            return $ appendToFirst c (s++[enclosers Map.! c]) decls
    else if elem c spaceChars
        then do
            s <- zeroOrMoreIn spaceChars
            decls <- separateDeclarationsHelper
            return $ appendToFirst c s decls
    else if c == backtick
        then do
            s <- zeroOrMoreIn wordChars
            decls <- separateDeclarationsHelper
            return $ appendToFirst c s decls
    else if elem c wordChars
        then checkForStartOfEqualsDecl c <|> readPossiblyEmptyWordAndContinue c
    else empty

separateDeclarations :: Parser [String]
separateDeclarations = fmap removeFirstIfEmpty separateDeclarationsHelper

declStringsToCode :: [String] -> Maybe CodeAST
declStringsToCode [] = Just []
declStringsToCode (a:b) = do
    (declA, remaining) <- parse equalsDeclaration a
    guard (remaining == "")
    declsB <- declStringsToCode b
    return (declA:declsB)

code :: Parser CodeAST
code = Parser (\cs -> do
    (strs, remaining) <- parse separateDeclarations cs
    decls <- declStringsToCode strs
    return (decls, remaining))
