module Uwecode.UweObj where

import qualified Data.Set as Set
import Numeric.Natural

type UweVar = Natural

data UweObj =
    UweObjFuncsList
        (Maybe Natural -> UweObj         ) {-simplify-}
        (UweObj -> UweObj                ) {-call-}
        (UweVar -> UweObj -> UweObj      ) {-replace-}
        (Set.Set UweVar                  ) {-allVars-}
        (Set.Set UweVar -> Set.Set UweVar) {-unboundVars-}
        (Set.Set UweVar -> UweObj        ) {-replaceBindings-}
        (UweObjEncoding                  ) {-asEncoding-}
    | Called UweObj UweObj
    | ArbitraryVal Natural

instance Eq UweObj where
	a == b = asEncoding a == asEncoding b

data UweObjEncoding =
    FuncsListEncoding String [UweObjEncoding]
    | CalledEncoding UweObjEncoding UweObjEncoding
    | ArbitraryValEncoding Natural deriving (Show, Eq)

decrementDepth :: Maybe Natural -> Maybe Natural
decrementDepth = (>>= (return . (subtract 1)))

simplify :: UweObj -> (Maybe Natural) -> UweObj
simplify (UweObjFuncsList f _ _ _ _ _ _) x = f x
simplify obj@(Called _ _) (Just 0) = obj
simplify obj@(Called a b) depth
    | bothValsEq  = obj
    | val1Eq      = simp val2
    | otherwise   = simp val1
    where
        simp x     = simplify x (decrementDepth depth)
        simpA      = simp a
        val1       = call a     b
        val2       = call simpA b
        val1Eq     = val1 == obj
        bothValsEq = val1Eq && val2 == obj
simplify obj@(ArbitraryVal _) _ = obj

call :: UweObj -> UweObj -> UweObj
call (UweObjFuncsList _ f _ _ _ _ _) x = f x
call obj@(Called _ _) obj2 = Called obj obj2
call obj@(ArbitraryVal _) obj2 = Called obj obj2

replace :: UweObj -> UweVar -> UweObj -> UweObj
replace (UweObjFuncsList _ _ f _ _ _ _) x y = f x y
replace (Called a b) m obj2 = Called (replace a m obj2) (replace b m obj2)
replace obj@(ArbitraryVal _) _ _ = obj

allVars :: UweObj -> Set.Set UweVar
allVars (UweObjFuncsList _ _ _ f _ _ _) = f
allVars (Called a b) = allVars a `Set.union` allVars b
allVars obj@(ArbitraryVal _) = Set.empty

unboundVars :: UweObj -> Set.Set UweVar -> Set.Set UweVar
unboundVars (UweObjFuncsList _ _ _ _ f _ _) x = f x
unboundVars (Called a b) vs = unboundVars a vs `Set.union` unboundVars b vs
unboundVars obj@(ArbitraryVal _) _ = Set.empty

replaceBindings :: UweObj -> Set.Set UweVar -> UweObj
replaceBindings (UweObjFuncsList _ _ _ _ _ f _) x = f x
replaceBindings (Called a b) vs = Called (replaceBindings a vs) (replaceBindings b vs)
replaceBindings obj@(ArbitraryVal _) _ = obj

asEncoding :: UweObj -> UweObjEncoding
asEncoding (UweObjFuncsList _ _ _ _ _ _ f) = f
asEncoding (Called a b) = CalledEncoding (asEncoding a) (asEncoding b)
asEncoding (ArbitraryVal n) = ArbitraryValEncoding n
