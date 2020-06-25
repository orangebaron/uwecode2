module Uwecode.BasicUweObjs where

import Uwecode.UweObj
import Numeric.Natural
import qualified Data.Set as Set
import Data.Maybe
import Data.List

returnVal :: UweVar -> UweObj
returnVal n = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _call = called me
    _replace m obj2
        | m == n = obj2
        | otherwise = me
    _allVars = Set.singleton n
    _unboundVars set
        | Set.member n set = Set.empty
        | otherwise = _allVars
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "returnVal" [n] []
    _asHsCode = "returnVal " ++ show n
    _replaceDeBruijn = const $ const me
    _toDeBruijn env = deBruijnReturnVal $ toEnum $ fromJust $ elemIndex n env
    _simplifyDeBruijn = const $ const me
    _incDeBruijn = const me

deBruijnReturnVal :: UweVar -> UweObj
deBruijnReturnVal n = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _call = called me
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "deBruijnReturnVal" [n] []
    _asHsCode = "deBruijnReturnVal " ++ show n
    _replaceDeBruijn n2 obj
        | n2 == n = obj
        | n > n2 = deBruijnReturnVal (n-1)
        | otherwise = me
    _toDeBruijn = const me
    _simplifyDeBruijn depth env = env !! fromEnum n
    _incDeBruijn threshold
        | n > threshold = deBruijnReturnVal $ n + 1
        | otherwise = me

function :: UweVar -> UweObj -> UweObj
function n x = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _call obj2
        | n `Set.member` vs = call (_replaceBindings vs) obj2
        | otherwise = replace (replaceBindings x vs) n obj2
        where vs = unboundVars obj2 Set.empty
    _replace m obj2
        | m == n = me
        | otherwise = function n $ replace x m obj2
    _allVars = Set.insert n $ allVars x
    _unboundVars vs = unboundVars x $ Set.insert n vs
    _replaceBindings vs
        | n `Set.member` vs = function newN $ replaceBindings newX vs
        | otherwise = function n $ replaceBindings x vs
        where
            newN = smallestValueNotIn $ vs `Set.union` _allVars
            newX = replace x n $ returnVal newN
    _asEncoding = UweObjEncoding "function" [n] [x]
    _asHsCode = "function " ++ show n ++ " (" ++ show x ++ ")"
    _replaceDeBruijn = const $ const me
    _toDeBruijn env = deBruijnFunction $ toDeBruijn x (n:env)
    _simplifyDeBruijn = const $ const me
    _incDeBruijn = const me
    
    smallestValueNotInHelper :: UweVar -> Set.Set UweVar -> UweVar
    smallestValueNotInHelper num set
        | num `Set.member` set = smallestValueNotInHelper (num+1) set
        | otherwise = num
    smallestValueNotIn :: Set.Set UweVar -> UweVar
    smallestValueNotIn = smallestValueNotInHelper 0

deBruijnFunction :: UweObj -> UweObj
deBruijnFunction x = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _call obj2 = replaceDeBruijn x 0 obj2
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "deBruijnFunction" [] [x]
    _asHsCode = "deBruijnFunction " ++ " (" ++ show x ++ ")"
    _replaceDeBruijn n obj = deBruijnFunction $ replaceDeBruijn x (n+1) (incDeBruijn obj 0)
    _toDeBruijn = const me
    _simplifyDeBruijn = const $ const me
    _incDeBruijn threshold = deBruijnFunction $ incDeBruijn x $ threshold + 1

called :: UweObj -> UweObj -> UweObj
called a b = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _call = called me
    _replace m obj2 = called (replace a m obj2) (replace b m obj2)
    _allVars = allVars a `Set.union` allVars b
    _unboundVars vs = unboundVars a vs `Set.union` unboundVars b vs
    _replaceBindings vs = called (replaceBindings a vs) (replaceBindings b vs)
    _asEncoding = UweObjEncoding "called" [] [a, b]
    _asHsCode = "called (" ++ show a ++ ") (" ++ show b ++ ")"
    _replaceDeBruijn n2 obj = called (replaceDeBruijn a n2 obj) (replaceDeBruijn b n2 obj)
    _toDeBruijn env = called (toDeBruijn a env) (toDeBruijn b env)
    _simplifyDeBruijn (Just 0) _ = me
    _simplifyDeBruijn depth env
        | useVal1   = simp2 val1
        | useVal2   = simp2 val2
        | useVal3   = simp2 val3
        | otherwise = me
        where
            simp1 x  = simplifyDeBruijn x (decrementDepth depth) env
            simp2 x  = simplifyDeBruijn x (decrementDepth depth) env
            simpA    = simp1 a
            simpB    = simp1 b
            val1     = call a b
            val2     = call simpA b
            val3     = call simpA simpB
            useVal1  = val1 /= me
            useVal2  = val2 /= me
            useVal3  = val3 /= me && encStrA == "arbitraryVal"
            (UweObjEncoding encStrA _ _) = asEncoding a
    _incDeBruijn threshold = called (incDeBruijn a threshold) (incDeBruijn b threshold)

churchNum :: Natural -> UweObj
churchNum n = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _simplify = const $ const me
    _call = calledChurchNum n
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "churchNum" [n] []
    _asHsCode = "churchNum " ++ show n
    _replaceDeBruijn = const $ const me
    _toDeBruijn = const me
    _simplifyDeBruijn = const $ const me
    _incDeBruijn = const me

calledChurchNum :: Natural -> UweObj -> UweObj
calledChurchNum 0 _ = function 0 $ returnVal 0
calledChurchNum n x = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _simplify = const $ const me
    _call = (call x) . (called $ calledChurchNum (n-1) x)
    _replace m obj2 = calledChurchNum n $ replace x m obj2
    _allVars = allVars x
    _unboundVars = unboundVars x
    _replaceBindings vs = calledChurchNum n $ replaceBindings x vs
    _asEncoding = UweObjEncoding "calledChurchNum" [n] [x]
    _asHsCode = "calledChurchNum " ++ show n ++ " (" ++ show x ++ ")"
    _replaceDeBruijn n2 obj = calledChurchNum n $ replaceDeBruijn x n2 obj
    _toDeBruijn env = calledChurchNum n $ toDeBruijn x env
    _simplifyDeBruijn = const $ const me
    _incDeBruijn = calledChurchNum n . incDeBruijn x

makeChurchTupObj :: UweObj -> UweObj -> UweObj
makeChurchTupObj x y = call (call (abs $ abs $ abs $ called (called (ret 0) (ret 2)) (ret 1)) x) y where
    abs = deBruijnFunction
    ret = deBruijnReturnVal

makeMaybeObj :: Maybe UweObj -> UweObj
makeMaybeObj Nothing = churchNum 0
makeMaybeObj (Just x) = call (abs $ abs $ abs $ called (ret 1) (ret 2)) x where
    abs = deBruijnFunction
    ret = deBruijnReturnVal

consListObj :: UweObj -> UweObj -> UweObj
consListObj head rest = obj where
    abs = deBruijnFunction
    ret = deBruijnReturnVal
    h = ret 3
    r = ret 2
    f = ret 1
    g = ret 0
    headCall = call f h
    restFold = call (call r f) g
    obj = call (call (abs $ abs $ abs $ abs $ called headCall restFold) head) rest

emptyListObj :: UweObj
emptyListObj = makeMaybeObj Nothing

makeListObj :: [UweObj] -> UweObj
makeListObj = foldr consListObj emptyListObj

makeBoolObj :: Bool -> UweObj
makeBoolObj a = abs $ abs $ ret (if a then 1 else 0) where
    abs = deBruijnFunction
    ret = deBruijnReturnVal

tupleChar :: Char -> UweObj
tupleChar chr = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _simplify = const $ const me
    _call = call meObj
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding ("tupleChar: "++[chr]) [] []
    _asHsCode = "tupleChar " ++ show chr
    _replaceDeBruijn = const $ const me
    _toDeBruijn = const me
    _simplifyDeBruijn = const $ const me
    _incDeBruijn = const me
    
    meObj :: UweObj
    meObj = makeListObj $ map (makeBoolObj . (== 1) . (`mod` 2) . (fromEnum chr `div`) . (2 ^)) [0..7]

tupleCharStr :: String -> UweObj
tupleCharStr "" = emptyListObj
tupleCharStr str@(chr:rest) = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _simplify = const $ const me
    _call = call meObj
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding ("tupleCharStr: "++str) [] []
    _asHsCode = "tupleCharStr " ++ show str
    _replaceDeBruijn = const $ const me
    _toDeBruijn = const me
    _simplifyDeBruijn = const $ const me
    _incDeBruijn = const me
    
    meObj :: UweObj
    meObj = consListObj (tupleChar chr) (tupleCharStr rest)

arbitraryVal :: Natural -> UweObj
arbitraryVal n = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyDeBruijn _incDeBruijn
    _simplify = const $ const me
    _call = called me
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "arbitraryVal" [n] []
    _asHsCode = "arbitraryVal " ++ show n
    _replaceDeBruijn = const $ const me
    _toDeBruijn = const me
    _simplifyDeBruijn = const $ const me
    _incDeBruijn = const me
