module Uwecode.BasicUweObjs where

import Uwecode.UweObj
import Numeric.Natural
import qualified Data.Set as Set

returnVal :: UweVar -> UweObj
returnVal n = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify = const me
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

function :: UweVar -> UweObj -> UweObj
function n x = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify (Just 0) = me
    _simplify depth = function n $ simplify x $ decrementDepth depth
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
            smallestValueNotInHelper :: UweVar -> Set.Set UweVar -> UweVar
            smallestValueNotInHelper num set
                | num `Set.member` set = smallestValueNotInHelper (num+1) set
                | otherwise = num
            smallestValueNotIn :: Set.Set UweVar -> UweVar
            smallestValueNotIn = smallestValueNotInHelper 0
            newN = smallestValueNotIn $ vs `Set.union` _allVars
            newX = replace x n $ returnVal newN
    _asEncoding = UweObjEncoding "function" [n] [x]
    _asHsCode = "function " ++ show n ++ " (" ++ show x ++ ")"

called :: UweObj -> UweObj -> UweObj
called a b = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify (Just 0) = me
    _simplify depth
        | val1Diff  = simp val1
        | val2Diff  = simp val2
        | val3Diff  = simp val3
        | otherwise = me
        where
            simp x     = simplify x (decrementDepth depth)
            simpA      = simp a
            simpB      = simp b
            val1       = call a     b
            val2       = call simpA b
            val3       = call simpA simpB
            val1Diff   = val1 /= me
            val2Diff   = val2 /= me
            val3Diff   = val3 /= me
    _call = called me
    _replace m obj2 = called (replace a m obj2) (replace b m obj2)
    _allVars = allVars a `Set.union` allVars b
    _unboundVars vs = unboundVars a vs `Set.union` unboundVars b vs
    _replaceBindings vs = called (replaceBindings a vs) (replaceBindings b vs)
    _asEncoding = UweObjEncoding "called" [] [a, b]
    _asHsCode = "called (" ++ show a ++ ") (" ++ show b ++ ")"

churchNum :: Natural -> UweObj
churchNum n = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify = const me
    _call = calledChurchNum n
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "churchNum" [n] []
    _asHsCode = "churchNum " ++ show n

calledChurchNum :: Natural -> UweObj -> UweObj
calledChurchNum 0 _ = function 0 $ returnVal 0
calledChurchNum n x = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify = const me
    _call = (call x) . (called $ calledChurchNum (n-1) x)
    _replace m obj2 = calledChurchNum n $ replace x m obj2
    _allVars = allVars x
    _unboundVars = unboundVars x
    _replaceBindings vs = calledChurchNum n $ replaceBindings x vs
    _asEncoding = UweObjEncoding "calledChurchNum" [n] [x]
    _asHsCode = "calledChurchNum " ++ show n ++ "(" ++ show x ++ ")"

makeChurchTupObj :: UweObj -> UweObj -> UweObj
makeChurchTupObj x y = function 0 $ called (called (returnVal 0) x) y

makeMaybeObj :: Maybe UweObj -> UweObj
makeMaybeObj Nothing = churchNum 0
makeMaybeObj (Just x) = function 0 $ function 1 $ called (returnVal 0) x

makeListObj :: UweObj -> UweObj -> UweObj
makeListObj head rest = makeMaybeObj $ Just $ makeChurchTupObj head rest

emptyListObj :: UweObj
emptyListObj = makeMaybeObj Nothing

makeBoolObj :: Bool -> UweObj
makeBoolObj a = function 0 $ function 1 $ returnVal (if a then 0 else 1)

tupleChar :: Char -> UweObj
tupleChar chr = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify = const me
    _call = call meObj
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding ("tupleChar: "++[chr]) [] []
    _asHsCode = "tupleChar " ++ show chr
    bools :: [UweObj]
    bools = map (makeBoolObj . (== 1) . (`mod` 2) . (fromEnum chr `div`) . (2 ^)) [0..7]
    boolTups :: [UweObj] -> [UweObj]
    boolTups l = map (\n -> (l !! (2*n + 1)) `makeChurchTupObj` (l !! (2*n))) [0..(length l `div` 2)]
    meObj :: UweObj
    meObj = head $ boolTups $ boolTups $ boolTups bools

tupleCharStr :: String -> UweObj
tupleCharStr "" = emptyListObj
tupleCharStr str@(chr:rest) = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify = const me
    _call = call meObj
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding ("tupleCharStr: "++str) [] []
    _asHsCode = "tupleCharStr " ++ show str
    meObj :: UweObj
    meObj = makeListObj (tupleChar chr) (tupleCharStr rest)

arbitraryVal :: Natural -> UweObj
arbitraryVal n = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode
    _simplify = const me
    _call = called me
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "arbitraryVal" [n] []
    _asHsCode = "arbitraryVal " ++ show n
