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
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
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
    _simplifyComb = const me
    _incDeBruijn = const me
    _asCombinators = (0, me)
    _asUnMemoizedComb = me

deBruijnReturnVal :: UweVar -> UweObj
deBruijnReturnVal n = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
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
    _simplifyComb = const me
    _incDeBruijn threshold
        | n > threshold = deBruijnReturnVal $ n + 1
        | otherwise = me
    _asCombinators
        | n == 0 = (1, combI)
        | otherwise = (m + 1, _benLynnCombF (0, combK) x) where x@(m, _) = asCombinators $ deBruijnReturnVal $ n - 1
    _asUnMemoizedComb = me

function :: UweVar -> UweObj -> UweObj
function n x = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
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
    _simplifyComb = const me
    _incDeBruijn = const me
    _asCombinators = (0, me)
    _asUnMemoizedComb = me
    
    smallestValueNotInHelper :: UweVar -> Set.Set UweVar -> UweVar
    smallestValueNotInHelper num set
        | num `Set.member` set = smallestValueNotInHelper (num+1) set
        | otherwise = num
    smallestValueNotIn :: Set.Set UweVar -> UweVar
    smallestValueNotIn = smallestValueNotInHelper 0

deBruijnFunction :: UweObj -> UweObj
deBruijnFunction x = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
    _call obj2 = replaceDeBruijn x 0 obj2
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "deBruijnFunction" [] [x]
    _asHsCode = "deBruijnFunction " ++ " (" ++ show x ++ ")"
    _replaceDeBruijn n obj = deBruijnFunction $ replaceDeBruijn x (n+1) (incDeBruijn obj 0)
    _toDeBruijn = const me
    _simplifyComb = const me
    _incDeBruijn threshold = deBruijnFunction $ incDeBruijn x $ threshold + 1
    _asCombinators = v where
        (n, e) = asCombinators x
        v = case n of
            0 -> (0, called combK e)
            _ -> (n - 1, e)
    _asUnMemoizedComb = me

called :: UweObj -> UweObj -> UweObj
called a b = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
    _call = called me
    _replace m obj2 = called (replace a m obj2) (replace b m obj2)
    _allVars = allVars a `Set.union` allVars b
    _unboundVars vs = unboundVars a vs `Set.union` unboundVars b vs
    _replaceBindings vs = called (replaceBindings a vs) (replaceBindings b vs)
    _asEncoding = UweObjEncoding "called" [] [a, b]
    _asHsCode = "called (" ++ show a ++ ") (" ++ show b ++ ")"
    _replaceDeBruijn n2 obj = called (replaceDeBruijn a n2 obj) (replaceDeBruijn b n2 obj)
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
    _incDeBruijn threshold = called (incDeBruijn a threshold) (incDeBruijn b threshold)
    _asCombinators = (max m n, _benLynnCombF x y) where
        x@(m, _) = asCombinators a
        y@(n, _) = asCombinators b
    _asUnMemoizedComb = called (asUnMemoizedComb a) (asUnMemoizedComb b)

churchNum :: Natural -> UweObj
churchNum n = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
    _call = calledChurchNum n
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _asEncoding = UweObjEncoding "churchNum" [n] []
    _asHsCode = "churchNum " ++ show n
    _replaceDeBruijn = const $ const me
    _toDeBruijn = const me
    _simplifyComb = const me
    _incDeBruijn = const me
    _asCombinators = (0, me)
    _asUnMemoizedComb = me

calledChurchNum :: Natural -> UweObj -> UweObj
calledChurchNum 0 _ = combI
calledChurchNum n x = me where
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
    _call = (call x) . (called $ calledChurchNum (n-1) x)
    _replace m obj2 = calledChurchNum n $ replace x m obj2
    _allVars = allVars x
    _unboundVars = unboundVars x
    _replaceBindings vs = calledChurchNum n $ replaceBindings x vs
    _asEncoding = UweObjEncoding "calledChurchNum" [n] [x]
    _asHsCode = "calledChurchNum " ++ show n ++ " (" ++ show x ++ ")"
    _replaceDeBruijn n2 obj = calledChurchNum n $ replaceDeBruijn x n2 obj
    _toDeBruijn env = calledChurchNum n $ toDeBruijn x env
    _simplifyComb = const me
    _incDeBruijn = calledChurchNum n . incDeBruijn x
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
    me = UweObj _call _replace _allVars _unboundVars _replaceBindings _asEncoding _asHsCode _replaceDeBruijn _toDeBruijn _simplifyComb _incDeBruijn _asCombinators _asUnMemoizedComb
    _replace = const $ const me
    _allVars = Set.empty
    _unboundVars = const Set.empty
    _replaceBindings = const me
    _replaceDeBruijn = const $ const me
    _toDeBruijn = const me
    _simplifyComb = const me
    _incDeBruijn = const me
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
