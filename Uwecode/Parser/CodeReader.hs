module Uwecode.Parser.CodeReader where

import Uwecode.AST
import Uwecode.Parser.Parser
import Uwecode.Parser.DeclarationParsers
import qualified Data.Map as Map
import Control.Monad

code :: Parser CodeAST
code = listed equalsDeclaration

varMap :: Parser GlobalVarMap
varMap = do
    codeAST <- code
    maybeReturn $ readCodeAST Map.empty codeAST

readUweString :: String -> Maybe GlobalVarMap
readUweString = takeFirstParse varMap
