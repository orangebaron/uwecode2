module Uwecode.UweObj where

import qualified Data.Set as Set
import Numeric.Natural

type UweVar = Natural
type Depth = Maybe Natural

data UweObj = UweObj {
    simplify        :: Depth -> UweObj,
    call            :: UweObj -> UweObj,
    replace         :: UweVar -> UweObj -> UweObj,
    allVars         :: Set.Set UweVar,
    unboundVars     :: Set.Set UweVar -> Set.Set UweVar,
    replaceBindings :: Set.Set UweVar -> UweObj,
    asEncoding      :: UweObjEncoding }

instance Eq UweObj where
    a == b = asEncoding a == asEncoding b

instance Show UweObj where
    show = show . asEncoding

data UweObjEncoding =
    FuncsListEncoding String [UweObj]
    | CalledEncoding UweObj UweObj
    | ArbitraryValEncoding Natural deriving (Show, Eq)

decrementDepth :: Depth -> Depth
decrementDepth = (>>= (return . (subtract 1)))
