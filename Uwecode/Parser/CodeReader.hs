module Uwecode.Parser.CodeReader where

import Uwecode.AST
import Uwecode.Parser.Parser
import Uwecode.Parser.DeclarationParsers
import qualified Data.Map as Map
import Control.Monad

code :: Parser CodeAST
code = Parser (\cs -> do
    (strs, remaining) <- parse separateDeclarations cs
    decls <- declStringsToCode strs
    return (decls, remaining))

readUweString :: String -> Maybe GlobalVarMap
readUweString str = do
    (codeAST, remaining) <- code `parse` str
    guard $ remaining == ""
    readCodeAST Map.empty codeAST
