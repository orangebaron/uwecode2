module Uwecode.UweObj where

import Numeric.Natural
import qualified Data.Set as Set

type UweVar = Natural
type Depth = Maybe Natural

infiniteDepth :: Depth
infiniteDepth = Nothing

{-
data UweObj = UweObj {
    call             :: UweObj -> UweObj,
    asEncoding       :: UweObjEncoding,
    asHsCode         :: String,
    toDeBruijn       :: [UweVar] -> UweObj,
    simplifyComb     :: Depth -> UweObj,
    asCombinators    :: (Natural, UweObj), -- have to convert to de bruijn first before doing this
    asUnMemoizedComb :: UweObj } -- this is getting way too complicated...
                                 -- sorry @ anyone whos reading this :)
-}

data UweObj = CombS | CombK | CombI | CombB | CombC | CalledUweObj UweObj UweObj | ArbitraryVal Natural |
    CustomUweObj (UweObj -> UweObj) {- call -} UweObjEncoding {- asEncoding -} String {- show -} ([UweVar] -> UweObj) {- toDeBruijn -} (Depth -> UweObj) {- simplify -} (Natural, UweObj) {- asCombinators -} UweObj {- asUnMemoizedCombinators -}

data UweObjEncoding = UweObjEncoding String [Natural] [UweObj] deriving (Show, Eq)

-------------------------

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
simplifyComb (CalledUweObj x y) depth = simplify (CalledUweObj (simplifyComb x depth) (simplifyComb y depth)) depth -- TODO this isnt even close
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
