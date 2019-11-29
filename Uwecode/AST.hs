module Uwecode.AST where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Control.Applicative
import Numeric.Natural
import Data.Map as Map
import Data.Maybe

data ExpressionAST = Word String | NumLiteral Natural | CharLiteral Char | StrLiteral String | FuncLiteral String ExpressionAST | Called ExpressionAST ExpressionAST deriving (Show)

data DeclarationAST = Equals Bool String ExpressionAST | Import FilePath String [String] deriving (Show)

type CodeAST = [DeclarationAST]

data GlobalVarMap = GlobalVarMap { privateVars :: Map String UweObj, publicVars :: Map String UweObj }

emptyGlobalVarMap :: GlobalVarMap
emptyGlobalVarMap = GlobalVarMap Map.empty Map.empty

getGlobalVar :: GlobalVarMap -> String -> Maybe UweObj
getGlobalVar (GlobalVarMap map1 map2) var = (if member var map2 then map2 else map1) !? var

setGlobalVar :: GlobalVarMap -> Bool -> String -> UweObj -> Maybe GlobalVarMap
setGlobalVar map@(GlobalVarMap map1 map2) isPublic var obj
    | isJust $ getGlobalVar map var = Nothing
    | isPublic  = Just $ GlobalVarMap map1 (insert var obj map2)
    | otherwise = Just $ GlobalVarMap (insert var obj map1) map2

data VarMap = VarMap { functionVars :: Map String UweVar, globalVars :: GlobalVarMap }

emptyVarMap = VarMap Map.empty emptyGlobalVarMap

getVar :: VarMap -> String -> Maybe UweObj
getVar (VarMap map1 map2) var = (fmap returnVal (map1 !? var)) <|> (getGlobalVar map2 var)

makeFunctionVar :: VarMap -> String -> (VarMap, UweVar)
makeFunctionVar (VarMap map1 map2) var = case (map1 !? var) of
    (Just n) -> ((VarMap map1 map2), n)
    Nothing  -> (VarMap (insert var n map1) map2, n) where
        n = firstNumNotInMap 0
        firstNumNotInMap testN
            | isIn testN = firstNumNotInMap (testN + 1)
            | otherwise  = testN
        isIn testN = not $ Map.null $ Map.filter (== testN) map1

readExpressionAST :: VarMap -> ExpressionAST -> Maybe UweObj
readExpressionAST map (Word var) = getVar map var
readExpressionAST map (NumLiteral n) = Just $ churchNum n
readExpressionAST map (CharLiteral c) = Just $ tupleChar c
readExpressionAST map (StrLiteral s) = Just $ tupleCharStr s
readExpressionAST map (FuncLiteral var exp) = (readExpressionAST newMap exp) >>= (return . function n) where (newMap, n) = makeFunctionVar map var
readExpressionAST map (Called exp1 exp2) = do
    obj1 <- readExpressionAST map exp1
    obj2 <- readExpressionAST map exp2
    return $ called obj1 obj2

readDeclarationAST :: GlobalVarMap -> DeclarationAST -> Maybe GlobalVarMap
readDeclarationAST map (Equals isPublic var exp) = do
    obj <- readExpressionAST (VarMap Map.empty map) exp
    newMap <- setGlobalVar map isPublic var obj
    return newMap

readCodeAST :: GlobalVarMap -> CodeAST -> Maybe GlobalVarMap
readCodeAST map [] = Just map
readCodeAST map (a:b) = do
    newMap <- readDeclarationAST map a
    readCodeAST newMap b
