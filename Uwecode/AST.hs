module Uwecode.AST where

import Numeric.Natural

data ExpressionAST = Word String | NumLiteral Natural | CharLiteral Char | StrLiteral String | FuncLiteral String ExpressionAST | Called ExpressionAST ExpressionAST deriving (Show)

data DeclarationAST = Equals String ExpressionAST deriving (Show)

type CodeAST = [DeclarationAST]
