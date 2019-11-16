module Uwecode.Parser.ExpressionParsers where

import Uwecode.Parser.Parser
import Uwecode.Parser.CharsAndStrings
import Uwecode.AST
import Control.Applicative

subNormalCallExpression :: Parser ExpressionAST
subNormalCallExpression = numLiteral <|> wordExpression

subBacktickCallExpression :: Parser ExpressionAST
subBacktickCallExpression = normalCall <|> subNormalCallExpression

expression :: Parser ExpressionAST
expression = funcLiteral <|> backtickCall <|> subBacktickCallExpression

funcLiteral :: Parser ExpressionAST
funcLiteral = separatedBy word arrowToken expression FuncLiteral

wordExpression :: Parser ExpressionAST
wordExpression = fmap Word word

numLiteral :: Parser ExpressionAST
numLiteral = fmap NumLiteral number

backtickCall :: Parser ExpressionAST
backtickCall = do
    a <- subBacktickCallExpression
    b <- backtickPrefacedWord
    c <- expression
    return $ Called (Called (Word b) a) c

normalCall :: Parser ExpressionAST
normalCall = do
    a <- subNormalCallExpression
    b <- expression
    return $ Called a b
