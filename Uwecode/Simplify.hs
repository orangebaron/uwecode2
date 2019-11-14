module Uwecode.Simplify where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Data.Maybe

type SimplifyCriteria a = UweObj -> Maybe a

simplifyWithCriteriaGivenDepth :: SimplifyCriteria a -> Depth -> UweObj -> Maybe a
simplifyWithCriteriaGivenDepth criteria depth obj = criteria $ simplify obj depth

simplifyWithCriteriaInfiniteDepth :: SimplifyCriteria a -> UweObj -> Maybe a
simplifyWithCriteriaInfiniteDepth criteria = simplifyWithCriteriaGivenDepth criteria Nothing

simplifyWithCriteriaGivenMaxDepth :: SimplifyCriteria a -> Depth -> UweObj -> Maybe a
simplifyWithCriteriaGivenMaxDepth criteria Nothing = simplifyWithCriteriaGivenDepth criteria Nothing
simplifyWithCriteriaGivenMaxDepth criteria maxDepth = helper (Just 2) where
    helper depth obj
        | depth >= maxDepth = simplifyWithCriteriaGivenDepth criteria maxDepth obj
        | isNothing criteriaReturn = helper increasedDepth simplifiedObj
        | otherwise = criteriaReturn
        where
            simplifiedObj = simplify obj depth
            criteriaReturn = criteria simplifiedObj
            increasedDepth = depth >>= (return . (* 2))
