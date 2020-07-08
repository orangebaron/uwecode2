module Uwecode.AST where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Control.Applicative
import Numeric.Natural
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.Trans.Maybe

data ExpressionAST = Word String | NumLiteral Natural | CharLiteral Char | StrLiteral String | FuncLiteral String ExpressionAST | Called ExpressionAST ExpressionAST deriving (Show)

data DeclarationAST = Equals Bool String ExpressionAST | Import FilePath String [String] deriving (Show)

type CodeAST = [DeclarationAST]

data GlobalVarMap = GlobalVarMap { privateVars :: M.Map String UweObj, publicVars :: M.Map String UweObj }

emptyGlobalVarMap :: GlobalVarMap
emptyGlobalVarMap = GlobalVarMap M.empty M.empty

getGlobalVar :: GlobalVarMap -> String -> Maybe UweObj
getGlobalVar (GlobalVarMap map1 map2) var = (if M.member var map2 then map2 else map1) M.!? var

setGlobalVar :: GlobalVarMap -> Bool -> String -> UweObj -> Maybe GlobalVarMap
setGlobalVar map@(GlobalVarMap map1 map2) isPublic var obj
    | isJust $ getGlobalVar map var = Nothing
    | isPublic  = Just $ GlobalVarMap map1 (M.insert var obj map2)
    | otherwise = Just $ GlobalVarMap (M.insert var obj map1) map2

data VarMap = VarMap { functionVars :: M.Map String UweVar, globalVars :: GlobalVarMap }

emptyVarMap = VarMap M.empty emptyGlobalVarMap

getVar :: VarMap -> String -> Maybe UweObj
getVar (VarMap map1 map2) var = (fmap returnVal (map1 M.!? var)) <|> (getGlobalVar map2 var)

makeFunctionVar :: VarMap -> String -> (VarMap, UweVar)
makeFunctionVar (VarMap map1 map2) var = case (map1 M.!? var) of
    (Just n) -> ((VarMap map1 map2), n)
    Nothing  -> (VarMap (M.insert var n map1) map2, n) where
        n = firstNumNotInMap 0
        firstNumNotInMap testN
            | isIn testN = firstNumNotInMap (testN + 1)
            | otherwise  = testN
        isIn testN = not $ M.null $ M.filter (== testN) map1

readExpressionAST :: VarMap -> ExpressionAST -> Maybe UweObj
readExpressionAST map (Word var) = getVar map var
readExpressionAST map (NumLiteral n) = Just $ churchNum n
readExpressionAST map (CharLiteral c) = Just $ tupleChar c
readExpressionAST map (StrLiteral s) = Just $ tupleCharStr s
readExpressionAST map (FuncLiteral var exp) = (readExpressionAST newMap exp) >>= (return . function n) where (newMap, n) = makeFunctionVar map var
readExpressionAST map (Called exp1 exp2) = do
    obj1 <- readExpressionAST map exp1
    obj2 <- readExpressionAST map exp2
    return $ CalledUweObj obj1 obj2
