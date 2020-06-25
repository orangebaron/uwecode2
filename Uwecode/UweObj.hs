module Uwecode.UweObj where

import Numeric.Natural
import qualified Data.Set as Set

type UweVar = Natural
type Depth = Maybe Natural

infiniteDepth :: Depth
infiniteDepth = Nothing

data UweObj = UweObj {
    call             :: UweObj -> UweObj,
    replace          :: UweVar -> UweObj -> UweObj,
    allVars          :: Set.Set UweVar,
    unboundVars      :: Set.Set UweVar -> Set.Set UweVar,
    replaceBindings  :: Set.Set UweVar -> UweObj,
    asEncoding       :: UweObjEncoding,
    asHsCode         :: String,
    replaceDeBruijn  :: UweVar -> UweObj -> UweObj,
    toDeBruijn       :: [UweVar] -> UweObj,
    simplifyDeBruijn :: Depth -> [UweObj] -> UweObj,
    incDeBruijn      :: UweVar -> UweObj }

simplify :: UweObj -> Depth -> UweObj
simplify obj depth = simplifyDeBruijn (toDeBruijn obj []) depth []

instance Eq UweObj where
    a == b = (asEncoding $ toDeBruijn a []) == (asEncoding $ toDeBruijn b [])

instance Show UweObj where
    show = asHsCode

data UweObjEncoding = UweObjEncoding String [Natural] [UweObj] deriving (Show, Eq)

decrementDepth :: Depth -> Depth
decrementDepth = fmap $ subtract 1

incrementDepth :: Depth -> Depth
incrementDepth = fmap $ (+ 1)
