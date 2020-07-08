module Uwecode.UweObj where

import Numeric.Natural
import qualified Data.Set as Set

type UweVar = Natural
type Depth = Maybe Natural

infiniteDepth :: Depth
infiniteDepth = Nothing

data UweObj = CombS | CombK | CombI | CombB | CombC | CalledUweObj UweObj UweObj | ArbitraryVal Natural |
    CustomUweObj (UweObj -> UweObj) {- call -} UweObjEncoding {- asEncoding -} String {- show -} ([UweVar] -> UweObj) {- toDeBruijn -} (Depth -> UweObj) {- simplify -} (Natural, UweObj) {- asCombinators -} UweObj {- asUnMemoizedCombinators -}

data UweObjEncoding = UweObjEncoding String [Natural] [UweObj] deriving (Show, Eq)

call :: UweObj -> UweObj -> UweObj
call (CalledUweObj (CalledUweObj CombS x) y) z = x `call` z `call` (y `call` z)
call (CalledUweObj CombK x) y = x
call CombI x = x
call (CalledUweObj (CalledUweObj CombB x) y) z = x `call` (y `call` z)
call (CalledUweObj (CalledUweObj CombC x) y) z = x `call` y `call` z
call (CustomUweObj f _ _ _ _ _ _) x = f x
call x y = CalledUweObj x y

asEncoding :: UweObj -> UweObjEncoding
asEncoding (CalledUweObj x y) = UweObjEncoding "CalledUweObj" [] [x, y]
asEncoding (ArbitraryVal n) = UweObjEncoding "ArbitraryVal" [n] []
asEncoding (CustomUweObj _ f _ _ _ _ _) = f
asEncoding x = UweObjEncoding (show x) [] []

instance Show UweObj where
    show CombS = "CombS"
    show CombK = "CombK"
    show CombI = "CombI"
    show CombB = "CombB"
    show CombC = "CombC"
    show (CalledUweObj x y) = "CalledUweObj (" ++ show x ++ ") (" ++ show y ++ ")"
    show (ArbitraryVal n) = "ArbitraryVal " ++ show n
    show (CustomUweObj _ _ f _ _ _ _) = f

toDeBruijn :: UweObj -> [UweVar] -> UweObj
toDeBruijn (CalledUweObj x y) env = CalledUweObj (toDeBruijn x env) (toDeBruijn y env)
toDeBruijn (CustomUweObj _ _ _ f _ _ _) env = f env
toDeBruijn x env = x

simplifyComb :: UweObj -> Depth -> UweObj
simplifyComb x (Just 0) = x
simplifyComb me@(CalledUweObj a b) depth
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
        useVal3  = val3 /= me && encStrA == "ArbitraryVal"
        (UweObjEncoding encStrA _ _) = asEncoding a
simplifyComb (CustomUweObj _ _ _ _ f _ _) depth = f depth
simplifyComb x depth = x

simplify :: UweObj -> Depth -> UweObj
simplify obj depth = simplifyComb (toCombinators obj) depth

asCombinators :: UweObj -> (Natural, UweObj)
asCombinators (CalledUweObj x y) = (max a c, CalledUweObj b d) where
    (a, b) = asCombinators x
    (c, d) = asCombinators y
asCombinators (CustomUweObj _ _ _ _ _ f _) = f
asCombinators x = (0, x)

asUnMemoizedCombinators :: UweObj -> UweObj
asUnMemoizedCombinators (CalledUweObj x y) = CalledUweObj (asUnMemoizedCombinators x) (asUnMemoizedCombinators y)
asUnMemoizedCombinators (CustomUweObj _ _ _ _ _ _ f) = f
asUnMemoizedCombinators x = x

toCombinators :: UweObj -> UweObj
toCombinators obj = asUnMemoizedCombinators $ snd $ asCombinators $ toDeBruijn obj []

instance Eq UweObj where
    a == b = (asEncoding a) == (asEncoding b)

decrementDepth :: Depth -> Depth
decrementDepth = fmap $ subtract 1

incrementDepth :: Depth -> Depth
incrementDepth = fmap (+ 1)
