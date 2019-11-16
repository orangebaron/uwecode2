module Uwecode.Simplify where

import Uwecode.UweObj
import Data.Maybe
import Data.Either

type Criteria a = UweObj -> Maybe a

applyCriteria :: Criteria a -> UweObj -> Either UweObj a
applyCriteria criteria obj = maybe (Left obj) Right $ criteria obj

simplifyWithCriteriaGivenDepth :: Criteria a -> Depth -> UweObj -> Either UweObj a
simplifyWithCriteriaGivenDepth criteria depth obj = applyCriteria criteria $ simplify obj depth

simplifyWithCriteriaInfiniteDepth :: Criteria a -> UweObj -> Either UweObj a
simplifyWithCriteriaInfiniteDepth criteria = simplifyWithCriteriaGivenDepth criteria Nothing

simplifyWithCriteriaGivenMaxDepth :: Criteria a -> Depth -> UweObj -> Either UweObj a
simplifyWithCriteriaGivenMaxDepth criteria Nothing = simplifyWithCriteriaInfiniteDepth criteria
simplifyWithCriteriaGivenMaxDepth criteria maxDepth = helper (Just 2) where
    helper depth obj
        | depth >= maxDepth = simplifyWithCriteriaGivenDepth criteria maxDepth obj
        | isLeft criteriaReturn = helper increasedDepth simplifiedObj
        | otherwise = criteriaReturn
        where
            simplifiedObj = simplify obj depth
            criteriaReturn = applyCriteria criteria simplifiedObj
            increasedDepth = fmap (* 2) depth
