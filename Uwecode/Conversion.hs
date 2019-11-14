module Uwecode.Conversion where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Uwecode.Simplify
import Numeric.Natural
import Data.Either

type Conversion a = Depth -> UweObj -> Either UweObj a
type IgnoringConversion a = Depth -> UweObj -> Maybe a

conversionToIgnoringConversion :: Conversion a -> IgnoringConversion a
conversionToIgnoringConversion conv depth obj = helper $ conv depth obj where
    helper (Left _) = Nothing
    helper (Right x) = Just x

objToNumber :: Conversion Natural
objToNumber depth obj
    | firstTryWorked = firstTry
    | secondTryWorked = secondTry
    | otherwise = thirdTry
    where
        firstTry = simplifyWithCriteriaGivenDepth firstCriteria depth obj
        firstTryWorked = isRight firstTry
        firstTryObj = fromLeft obj firstTry -- default value obj should never be used but is here just in case
        secondTry = simplifyWithCriteriaGivenDepth secondCriteria depth $ call firstTryObj $ arbitraryVal 0
        secondTryWorked = isRight secondTry
        secondTryObj = fromLeft obj secondTry -- default value obj should never be used but is here just in case
        thirdTry = simplifyWithCriteriaGivenDepth thirdCriteria depth $ call secondTryObj $ arbitraryVal 1

        makeCriteria :: (UweObjEncoding -> Maybe Natural) -> SimplifyCriteria Natural
        makeCriteria helper obj = helper $ asEncoding obj

        firstCriteria :: SimplifyCriteria Natural
        firstCriteria = makeCriteria firstHelper
        firstHelper (EtcEncoding "churchNum" [n] []) = Just n
        firstHelper _ = Nothing

        secondCriteria :: SimplifyCriteria Natural
        secondCriteria = makeCriteria secondHelper
        secondHelper (EtcEncoding "calledChurchNum" [n] [x])
            | x == arbitraryVal 0 = Just n
            | otherwise = Nothing
        secondHelper _ = Nothing

        thirdCriteria :: SimplifyCriteria Natural
        thirdCriteria = makeCriteria thirdHelper
        thirdHelper (CalledEncoding a b)
            | b == arbitraryVal 1 = (secondCriteria a) >>= (return . (+ 1))
            | a == arbitraryVal 0 = (thirdCriteria b) >>= (return . (+ 1))
            | otherwise = Nothing
        thirdHelper (ArbitraryValEncoding 1) = Just 0
        thirdHelper _ = Nothing
