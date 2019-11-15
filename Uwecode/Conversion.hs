module Uwecode.Conversion where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Uwecode.Simplify
import GHC.Natural
import Data.Either

type Conversion a = Depth -> UweObj -> Either UweObj a
type IgnoringConversion a = Depth -> UweObj -> Maybe a

conversionToIgnoringConversion :: Conversion a -> IgnoringConversion a
conversionToIgnoringConversion conv depth obj = helper $ conv depth obj where
    helper (Left _) = Nothing
    helper (Right x) = Just x

criteriaToConversion :: [Maybe (SimplifyCriteria a)] -> Conversion a
criteriaToConversion (firstCriteria:restCriteria) depth obj0 = tryToSimplify (helper 0) firstCriteria obj0 where
    tryToSimplify nextHelper Nothing obj = nextHelper obj
    tryToSimplify nextHelper (Just criteria) obj = either nextHelper Right $ simplifyWithCriteriaGivenMaxDepth criteria depth obj
    helper n obj
        | n >= length restCriteria = Left obj0
        | otherwise = tryToSimplify (helper $ n + 1) (restCriteria !! n) (called obj $ arbitraryVal $ intToNatural n)

objToNumber :: Conversion Natural
objToNumber = criteriaToConversion [Just criteria1, Just criteria2, Just criteria3] where
    makeCriteria :: (UweObjEncoding -> Maybe Natural) -> SimplifyCriteria Natural
    makeCriteria helper obj = helper $ asEncoding obj

    criteria1 :: SimplifyCriteria Natural
    criteria1 = makeCriteria helper1
    helper1 (EtcEncoding "churchNum" [n] []) = Just n
    helper1 _ = Nothing

    criteria2 :: SimplifyCriteria Natural
    criteria2 = makeCriteria helper2
    helper2 (EtcEncoding "calledChurchNum" [n] [x])
            | x == arbitraryVal 0 = Just n
            | otherwise = Nothing
    helper2 _ = Nothing

    criteria3 :: SimplifyCriteria Natural
    criteria3 = makeCriteria helper3
    helper3 (CalledEncoding a b)
            | a == arbitraryVal 0 = (criteria3 b) >>= (return . (+ 1))
            | b == arbitraryVal 1 = criteria2 a
            | otherwise = Nothing
    helper3 (ArbitraryValEncoding 1) = Just 0
    helper3 _ = Nothing
