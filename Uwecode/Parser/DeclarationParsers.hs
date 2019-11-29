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
declaration = equalsDeclaration <|> privateEqualsDeclaration

equalsDeclaration :: Parser DeclarationAST
equalsDeclaration = separatedBy word (stringToken equalsStr) expression (Equals True)

privateEqualsDeclaration :: Parser DeclarationAST
privateEqualsDeclaration = separatedBy word (stringToken privateEqualsStr) expression (Equals False)
