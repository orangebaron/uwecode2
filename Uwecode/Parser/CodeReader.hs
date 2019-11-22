module Uwecode.Parser.CodeReader where

import Uwecode.AST
import Uwecode.Parser.Parser
import Uwecode.Parser.DeclarationParsers
import qualified Data.Map as Map
import Control.Monad

code :: Parser CodeAST
code = listed equalsDeclaration

readUweString :: String -> Maybe GlobalVarMap
readUweString str = do
    codeAST <- code `takeFirstParse` str
    readCodeAST Map.empty codeAST
