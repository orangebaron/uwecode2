module Uwecode.Parser.DeclarationParsers where

import Uwecode.Parser.Parser
import Uwecode.Parser.CharsAndStrings
import Uwecode.Parser.ExpressionParsers
import Uwecode.Parser.Grouping
import Uwecode.AST
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad

declaration :: Parser DeclarationAST
declaration = equalsDeclaration <|> privateEqualsDeclaration <|> importDeclaration

equalsDeclaration :: Parser DeclarationAST
equalsDeclaration = separatedBy word (stringToken equalsStr) expression (Equals True)

privateEqualsDeclaration :: Parser DeclarationAST
privateEqualsDeclaration = separatedBy word (stringToken privateEqualsStr) expression (Equals False)

importDeclaration :: Parser DeclarationAST
importDeclaration = token $ do
    specificChar '{'
    enclosed <- enclosedParser '}'
    returnGoodFromParse insideImportDeclaration enclosed

insideImportDeclaration :: Parser DeclarationAST
insideImportDeclaration = do
    file <- token nonSpaces
    prefix <- optionally $ stringToken importPrefixStr >> word
    specific <- optionally $ stringToken importSpecificStr >> oneOrMoreListed word
    return $ Import file (maybe "" id prefix) (maybe [] id specific)
