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

switchEither :: Either a b -> Either b a
switchEither (Left x) = Right x
switchEither (Right x) = Left x

objToNumber :: Conversion Natural
objToNumber depth obj = switchEither $ do
	firstTryObj <- doSimplify firstCriteria obj
	secondTryObj <- doSimplify secondCriteria $ call firstTryObj $ arbitraryVal 0
	doSimplify thirdCriteria $ call secondTryObj $ arbitraryVal 1
	return firstTryObj
	where
		doSimplify :: SimplifyCriteria Natural -> UweObj -> Either Natural UweObj
		doSimplify criteria x = switchEither $ simplifyWithCriteriaGivenDepth criteria depth x

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
			| a == arbitraryVal 0 = (thirdCriteria b) >>= (return . (+ 1))
			| b == arbitraryVal 1 = secondCriteria a
			| otherwise = Nothing
		thirdHelper (ArbitraryValEncoding 1) = Just 0
		thirdHelper _ = Nothing
