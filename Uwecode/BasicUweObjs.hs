module Uwecode.BasicUweObjs where

import Uwecode.UweObj
import Numeric.Natural
import qualified Data.Set as Set

returnVal :: UweVar -> UweObj
returnVal n = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding
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
    _asEncoding = FuncsListEncoding ("returnVal " ++ show n) []

function :: UweVar -> UweObj -> UweObj
function n x = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding
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
    _asEncoding = FuncsListEncoding ("function " ++ show n) [x]

called :: UweObj -> UweObj -> UweObj
called a b = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding
    _simplify (Just 0) = me
    _simplify depth
        | bothValsEq  = me
        | val1Eq      = simp val2
        | otherwise   = simp val1
        where
            simp x     = simplify x (decrementDepth depth)
            simpA      = simp a
            val1       = call a     b
            val2       = call simpA b
            val1Eq     = val1 == me
            bothValsEq = val1Eq && val2 == me
    _call = called me
    _replace m obj2 = called (replace a m obj2) (replace b m obj2)
    _allVars = allVars a `Set.union` allVars b
    _unboundVars vs = unboundVars a vs `Set.union` unboundVars b vs
    _replaceBindings vs = called (replaceBindings a vs) (replaceBindings b vs)
    _asEncoding = CalledEncoding a b

churchNum :: Natural -> UweObj
churchNum n = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding
    _simplify = const me
    _call = calledChurchNum n
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = FuncsListEncoding ("churchNum " ++ show n) []

calledChurchNum :: Natural -> UweObj -> UweObj
calledChurchNum 0 _ = function 0 $ returnVal 0
calledChurchNum n x = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding
    _simplify = const me
    _call = (call x) . (called $ calledChurchNum (n-1) x)
    _replace m obj2 = calledChurchNum n $ replace x m obj2
    _allVars = allVars x
    _unboundVars = unboundVars x
    _replaceBindings vs = calledChurchNum n $ replaceBindings x vs
    _asEncoding = FuncsListEncoding ("calledChurchNum " ++ show n) [x]

arbitraryVal :: Natural -> UweObj
arbitraryVal n = me where
    me = UweObj _simplify _call _replace _allVars _unboundVars _replaceBindings _asEncoding
    _simplify = const me
    _call = called me
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = ArbitraryValEncoding n
