module Uwecode.Parser.ExpressionParsers where

import Uwecode.Parser.Parser
import Uwecode.Parser.CharsAndStrings
import Uwecode.Parser.Grouping
import Uwecode.AST
import Control.Applicative
import Prelude.SafeEnum as SE
import Text.Read
import qualified Data.Map as Map

subNormalCallExpression :: Parser ExpressionAST
subNormalCallExpression = quotedLiteralExpression <|> numLiteral <|> wordExpression

subBacktickCallExpression :: Parser ExpressionAST
subBacktickCallExpression = normalCall <|> subNormalCallExpression

expression :: Parser ExpressionAST
expression = parenEnclosedExpressionCall <|> parenEnclosedExpression <|> funcLiteral <|> backtickCall <|> subBacktickCallExpression

funcLiteral :: Parser ExpressionAST
funcLiteral = separatedBy word arrowToken expression FuncLiteral

wordExpression :: Parser ExpressionAST
wordExpression = do
    w <- word
    if w == "=" then empty else return (Word w)

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

parenEnclosedExpression :: Parser ExpressionAST
parenEnclosedExpression = token $ do
    specificChar '('
    enclosed <- enclosedParser $ EnclosedState ')'
    case (expression `takeFirstParse` enclosed) of
        (Just expr) -> return expr
        _           -> empty

fixString :: String -> String
fixString ('\\':'x':a:b:c:s) = (maybe ("\\x"++(a:b:c:"")) id $ do
    n <- readMaybe $ a:b:c:"" :: Maybe Int
    chr <- SE.toEnum n :: Maybe Char
    return [chr] ) ++ (fixString s)
fixString ('\\':c:s) = (Map.findWithDefault c c backslashReplacements) : s
fixString (c:s) = c:(fixString s)
fixString "" = ""

makeQuotedExpr :: String -> Bool -> Maybe ExpressionAST
makeQuotedExpr s True = Just $ StrLiteral $ fixString s
makeQuotedExpr s False = case (fixString s) of
    [c] -> Just $ CharLiteral c
    _   -> Nothing

quotedLiteralExpression :: Parser ExpressionAST
quotedLiteralExpression = token $ do
    c <- charSatisfies (`elem` quotes)
    s <- quotedParser $ QuotedState (c == doubleQuote) False
    case (makeQuotedExpr s (c == doubleQuote)) of
        (Just x) -> return x
        Nothing  -> empty

parenEnclosedExpressionCall :: Parser ExpressionAST
parenEnclosedExpressionCall = do
    a <- parenEnclosedExpression
    b <- expression
    return $ Called a b
