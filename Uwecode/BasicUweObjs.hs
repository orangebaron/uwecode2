module Uwecode.BasicUweObjs where

import Uwecode.UweObj
import Numeric.Natural
import qualified Data.Set as Set
import Data.Maybe
import Data.List

_benLynnCombF :: (Natural, UweObj) -> (Natural, UweObj) -> UweObj
_benLynnCombF (a, x) (b, y) = case (a, b) of
    (0, 0)             -> x `called` y
    (0, n)             -> combBn n `called` x `called` y
    (n, 0)             -> combCn n `called` x `called` y
    (n, m) | n == m    -> combSn n `called` x `called` y
           | n < m     -> combBn (m - n) `called` (combSn n `called` x) `called` y
           | otherwise -> combCn (n - m) `called` (combBn (n - m) `called` combSn m `called` x) `called` y

returnVal :: UweVar -> UweObj
returnVal n = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _call = called me
    _asEncoding = UweObjEncoding "returnVal" [n] []
    _asHsCode = "returnVal " ++ show n
    _toDeBruijn env = deBruijnReturnVal $ toEnum $ fromJust $ elemIndex n env
    _simplifyComb = const me
    _asCombinators = (0, me)
    _asUnMemoizedComb = me

deBruijnReturnVal :: UweVar -> UweObj
deBruijnReturnVal n = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _call = called me
    _asEncoding = UweObjEncoding "deBruijnReturnVal" [n] []
    _asHsCode = "deBruijnReturnVal " ++ show n
    _toDeBruijn = const me
    _simplifyComb = const me
    _asCombinators
        | n == 0 = (1, combI)
        | otherwise = (m + 1, _benLynnCombF (0, combK) x) where x@(m, _) = asCombinators $ deBruijnReturnVal $ n - 1
    _asUnMemoizedComb = me

function :: UweVar -> UweObj -> UweObj
function n x = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _call = const me
    _asEncoding = UweObjEncoding "function" [n] [x]
    _asHsCode = "function " ++ show n ++ " (" ++ show x ++ ")"
    _toDeBruijn env = deBruijnFunction $ toDeBruijn x (n:env)
    _simplifyComb = const me
    _asCombinators = (0, me)
    _asUnMemoizedComb = me

deBruijnFunction :: UweObj -> UweObj
deBruijnFunction x = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _call = const me
    _asEncoding = UweObjEncoding "deBruijnFunction" [] [x]
    _asHsCode = "deBruijnFunction " ++ " (" ++ show x ++ ")"
    _toDeBruijn = const me
    _simplifyComb = const me
    _asCombinators = v where
        (n, e) = asCombinators x
        v = case n of
            0 -> (0, called combK e)
            _ -> (n - 1, e)
    _asUnMemoizedComb = me

called :: UweObj -> UweObj -> UweObj
called a b = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _call = called me
    _asEncoding = UweObjEncoding "called" [] [a, b]
    _asHsCode = "called (" ++ show a ++ ") (" ++ show b ++ ")"
    _toDeBruijn env = called (toDeBruijn a env) (toDeBruijn b env)
    _simplifyComb (Just 0) = me
    _simplifyComb depth
        | useVal1   = simp val1
        | useVal2   = simp val2
        | useVal3   = simp val3
        | otherwise = val3
        where
            simp x  = simplifyComb x (decrementDepth depth)
            simpA    = simp a
            simpB    = simp b
            val1     = call a b
            val2     = call simpA b
            val3     = call simpA simpB
            useVal1  = val1 /= me
            useVal2  = val2 /= me
            useVal3  = val3 /= me && encStrA == "arbitraryVal"
            (UweObjEncoding encStrA _ _) = asEncoding a
    _asCombinators = (max m n, _benLynnCombF x y) where
        x@(m, _) = asCombinators a
        y@(n, _) = asCombinators b
    _asUnMemoizedComb = called (asUnMemoizedComb a) (asUnMemoizedComb b)

churchNum :: Natural -> UweObj
churchNum n = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _call = calledChurchNum n
    _asEncoding = UweObjEncoding "churchNum" [n] []
    _asHsCode = "churchNum " ++ show n
    _toDeBruijn = const me
    _simplifyComb = const me
    _asCombinators = (0, me)
    _asUnMemoizedComb = me

calledChurchNum :: Natural -> UweObj -> UweObj
calledChurchNum 0 _ = combI
calledChurchNum n x = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _call = (call x) . (called $ calledChurchNum (n-1) x)
    _asEncoding = UweObjEncoding "calledChurchNum" [n] [x]
    _asHsCode = "calledChurchNum " ++ show n ++ " (" ++ show x ++ ")"
    _toDeBruijn env = calledChurchNum n $ toDeBruijn x env
    _simplifyComb = const me
    _asCombinators = (m, calledChurchNum n y) where (m, y) = asCombinators x
    _asUnMemoizedComb = calledChurchNum n $ asUnMemoizedComb x

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

reallySimpleObj :: UweObjEncoding -> String -> (UweObj -> UweObj) -> (UweObj -> UweObj) -> UweObj
reallySimpleObj _asEncoding _asHsCode _call _asUnMemoizedCombF = me where
    me = UweObj _call _asEncoding _asHsCode _toDeBruijn _simplifyComb _asCombinators _asUnMemoizedComb
    _toDeBruijn = const me
    _simplifyComb = const me
    _asCombinators = (0, me)
    _asUnMemoizedComb = _asUnMemoizedCombF me

tupleChar :: Char -> UweObj
tupleChar chr = reallySimpleObj (UweObjEncoding ("tupleChar: "++[chr]) [] []) ("tupleChar " ++ show chr) (call $ toCombinators $ makeListObj $ map (makeBoolObj . (== 1) . (`mod` 2) . (fromEnum chr `div`) . (2 ^)) [0..7]) id

tupleCharStr :: String -> UweObj
tupleCharStr "" = emptyListObj
tupleCharStr str@(chr:rest) = reallySimpleObj (UweObjEncoding ("tupleCharStr: "++str) [] []) ("tupleCharStr " ++ show str) (call $ toCombinators $ consListObj (tupleChar chr) (tupleCharStr rest)) id

arbitraryVal :: Natural -> UweObj
arbitraryVal n = reallySimpleObj (UweObjEncoding "arbitraryVal" [n] []) ("arbitraryVal " ++ show n) (called $ arbitraryVal n) id

combI :: UweObj
combI = reallySimpleObj (UweObjEncoding "combI" [] []) "combI" id id

combK :: UweObj
combK = reallySimpleObj (UweObjEncoding "combK" [] []) "combK" calledCombK id
calledCombK x = reallySimpleObj (UweObjEncoding "calledCombK" [] [x]) ("calledCombK (" ++ show x ++ ")") (const x) (const $ calledCombK $ asUnMemoizedComb x)

tripleCombinator :: Char -> (UweObj -> UweObj -> UweObj -> UweObj) -> UweObj
tripleCombinator c f = me where
    name1 = "comb" ++ [c]
    name2 = "calledComb" ++ [c]
    name3 = "calledCalledComb" ++ [c]
    me = reallySimpleObj (UweObjEncoding name1 [] []) name1 calledMe id
    calledMe x = reallySimpleObj (UweObjEncoding name2 [] [x]) (name2 ++ " (" ++ show x ++ ")") (calledCalledMe x) id
    calledCalledMe x y = reallySimpleObj (UweObjEncoding name3 [] [x]) (name3 ++ " (" ++ show x ++ ") (" ++ show y ++ ")") (f x y) id

combB = tripleCombinator 'B' (\x y z -> x `call` (y `call` z))
combC = tripleCombinator 'C' (\x y z -> x `call` z `call` y)
combS = tripleCombinator 'S' (\x y z -> x `call` z `call` (y `call` z))

nCombinator :: Char -> (Natural -> UweObj) -> (Natural -> UweObj)
nCombinator c f = combN where
    name = "comb" ++ [c] ++ "n"
    combN n = reallySimpleObj (UweObjEncoding name [n] []) (name ++ " " ++ show n) (called $ combN n) (const $ f n)

combBn = nCombinator 'B' (\n -> iterate ((combB `called` combB) `called`) combB !! (fromInteger $ toInteger $ n - 1))
combCn = nCombinator 'C' (\n -> iterate ((combB `called` (combB `called` combC) `called` combB) `called`) combC !! (fromInteger $ toInteger $ n - 1))
combSn = nCombinator 'S' (\n -> iterate ((combB `called` (combB `called` combS) `called` combB) `called`) combS !! (fromInteger $ toInteger $ n - 1))
