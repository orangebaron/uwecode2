module Uwecode.Conversion where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Uwecode.Simplify
import Uwecode.IO
import GHC.Natural
import Data.Either

type Conversion a = Depth -> UweObj -> Either UweObj a
type IgnoringConversion a = Depth -> UweObj -> Maybe a
type EncodingCriteria a = UweObjEncoding -> Maybe a

ignoringConversion :: Conversion a -> IgnoringConversion a
ignoringConversion conv depth obj = either (const Nothing) Just $ conv depth obj

criteriaToConversion :: [Maybe (Criteria a)] -> Conversion a
criteriaToConversion (firstCriteria:restCriteria) depth obj0 = tryToSimplify (helper 0) firstCriteria obj0 where
    tryToSimplify nextHelper Nothing obj = nextHelper obj
    tryToSimplify nextHelper (Just criteria) obj = either nextHelper Right $ simplifyWithCriteriaGivenMaxDepth criteria depth obj
    helper n obj
        | n >= length restCriteria = Left obj0
        | otherwise = tryToSimplify (helper $ n + 1) (restCriteria !! n) (called obj $ arbitraryVal $ intToNatural n)

encodingCriteriaToCriteria :: EncodingCriteria a -> Criteria a
encodingCriteriaToCriteria = (. asEncoding)

encodingCriteriaToConversion :: [Maybe (EncodingCriteria a)] -> Conversion a
encodingCriteriaToConversion = criteriaToConversion . (map $ fmap encodingCriteriaToCriteria)

protectConversionEither :: (a -> Either a b) -> a -> Either a b
protectConversionEither f x = Left x <> f x

encCriteriaAndEitherFuncToConversion :: EncodingCriteria a -> (Depth -> UweObj -> Either UweObj a) -> Conversion a
encCriteriaAndEitherFuncToConversion criteria func depth obj = either (protectConversionEither $ func depth) Right $ simplifyWithCriteriaGivenMaxDepth (encodingCriteriaToCriteria criteria) depth obj

objToNumber :: Conversion Natural
objToNumber = encodingCriteriaToConversion [Just criteria1, Just criteria2, Just criteria3] where
    criteria1 :: EncodingCriteria Natural
    criteria2 :: EncodingCriteria Natural
    criteria3 :: EncodingCriteria Natural

    criteria1 (UweObjEncoding "churchNum" [n] []) = Just n
    criteria1 _ = Nothing

    criteria2 (UweObjEncoding "calledChurchNum" [n] [x])
            | x == arbitraryVal 0 = Just n
            | otherwise = Nothing
    criteria2 _ = Nothing

    criteria3 (UweObjEncoding "called" [] [a, b])
            | a == arbitraryVal 0 = (criteria3 $ asEncoding b) >>= (return . (+ 1))
            | b == arbitraryVal 1 = criteria2 $ asEncoding a
            | otherwise = Nothing
    criteria3 (UweObjEncoding "arbitraryVal" [1] []) = Just 0
    criteria3 _ = Nothing

objToBool :: Conversion Bool
objToBool = encodingCriteriaToConversion [Just criteria1, Nothing, Just criteria2] where
    criteria1 :: EncodingCriteria Bool
    criteria2 :: EncodingCriteria Bool

    criteria1 (UweObjEncoding "churchNum" [0] []) = Just False
    criteria1 (UweObjEncoding "true" [] []) = Just True
    criteria1 _ = Nothing

    criteria2 (UweObjEncoding "arbitraryVal" [n] []) = Just $ n == 0
    criteria2 _ = Nothing

objToTuple :: Conversion (UweObj, UweObj)
objToTuple = encodingCriteriaToConversion [Just criteria1, Just criteria2] where
    criteria1 :: EncodingCriteria (UweObj, UweObj)
    criteria2 :: EncodingCriteria (UweObj, UweObj)

    criteria1 (UweObjEncoding "tuple" [] [a, b]) = Just (a, b)
    criteria1 _ = Nothing

    criteria2 (UweObjEncoding "called" [] [a, b]) = result $ asEncoding a where
        result (UweObjEncoding "called" [] [c, d]) = result2 (asEncoding c) d
        result _ = Nothing
        result2 (UweObjEncoding "arbitraryVal" [0] []) d = Just (d, b)
        result2 _ _ = Nothing
    criteria2 _ = Nothing

objToMaybe :: Conversion (Maybe UweObj)
objToMaybe = encodingCriteriaToConversion [Just criteria1, Nothing, Just criteria2] where
    criteria1 :: EncodingCriteria (Maybe UweObj)
    criteria2 :: EncodingCriteria (Maybe UweObj)

    criteria1 (UweObjEncoding "churchNum" [0] []) = Just Nothing
    criteria1 (UweObjEncoding "left" [] [a]) = Just $ Just a
    criteria1 _ = Nothing

    criteria2 (UweObjEncoding "arbitraryVal" [1] []) = Just Nothing
    criteria2 (UweObjEncoding "called" [] [a, b]) = result $ asEncoding a where
        result (UweObjEncoding "arbitraryVal" [0] []) = Just $ Just b
        result _ = Nothing
    criteria2 _ = Nothing

objToEither :: Conversion (Either UweObj UweObj)
objToEither = encodingCriteriaToConversion [Just criteria1, Nothing, Just criteria2] where
    criteria1 :: EncodingCriteria (Either UweObj UweObj)
    criteria2 :: EncodingCriteria (Either UweObj UweObj)

    criteria1 (UweObjEncoding "left" [] [a]) = Just $ Left a
    criteria1 (UweObjEncoding "right" [] [a]) = Just $ Right a
    criteria1 _ = Nothing

    criteria2 (UweObjEncoding "called" [] [a, b]) = result $ asEncoding a where
        result (UweObjEncoding "arbitraryVal" [0] []) = Just $ Left b
        result (UweObjEncoding "arbitraryVal" [1] []) = Just $ Right b
        result _ = Nothing
    criteria2 _ = Nothing

objToChar :: Conversion Char
objToChar = encCriteriaAndEitherFuncToConversion criteria helper where
    criteria :: EncodingCriteria Char
    helper :: Depth -> UweObj -> Either UweObj Char
    convToInt :: [Bool] -> Int

    criteria (UweObjEncoding "char" [n] []) = Just $ toEnum $ naturalToInt n
    criteria _ = Nothing

    helper depth objSimp = do
        (l, r)     <- objToTuple depth objSimp
        (ll, lr)   <- objToTuple depth l
        (rl, rr)   <- objToTuple depth r
        (lll, llr) <- objToTuple depth ll
        (lrl, lrr) <- objToTuple depth lr
        (rll, rlr) <- objToTuple depth rl
        (rrl, rrr) <- objToTuple depth rr
        lllB <- objToBool depth lll
        llrB <- objToBool depth llr
        lrlB <- objToBool depth lrl
        lrrB <- objToBool depth lrr
        rllB <- objToBool depth rll
        rlrB <- objToBool depth rlr
        rrlB <- objToBool depth rrl
        rrrB <- objToBool depth rrr
        return $ toEnum $ convToInt [rrrB, rrlB, rlrB, rllB, lrrB, lrlB, llrB, lllB]

    convToInt bools = sum $ [(2 ^ i) * (fromEnum $ bools !! i) | i <- [0..(length bools - 1)]]

objToList :: Conversion [UweObj]
objToList = encCriteriaAndEitherFuncToConversion criteria helper where
    criteria :: EncodingCriteria [UweObj]
    helper :: Depth -> UweObj -> Either UweObj [UweObj]
    maybeObjToList :: Depth -> Maybe UweObj -> Either UweObj [UweObj]

    criteria (UweObjEncoding "list" [] list) = Just list
    criteria _ = Nothing

    helper depth objSimp = do
        maybe <- objToMaybe depth objSimp
        maybeObjToList depth maybe

    maybeObjToList _ Nothing = Right []
    maybeObjToList depth (Just x) = do
        (h, t) <- objToTuple depth x
        restOfList <- objToList depth t
        return (h:restOfList)

objToString :: Conversion String
objToString = encCriteriaAndEitherFuncToConversion criteria helper where
    criteria :: EncodingCriteria String
    helper :: Depth -> UweObj -> Either UweObj String
    listToStr :: Depth -> [UweObj] -> Either UweObj String

    criteria (UweObjEncoding objStr [] [])
        | take lenStr objStr == str = Just $ drop lenStr objStr
        | otherwise = Nothing
        where
            str = "string "
            lenStr = length str

    helper depth objSimp = do
        list <- objToList depth objSimp
        listToStr depth list

    listToStr _ [] = Right ""
    listToStr depth (c:s) = do
        chr <- objToChar depth c
        str <- listToStr depth s
        return (chr:str)

objToIO :: IgnoringConversion UweIO
objToIO depth obj = do
    either1 <- ignoringConversion objToEither depth obj
    case either1 of
        (Left obj2) -> do
            either2 <- ignoringConversion objToEither depth obj2
            case either2 of
                (Left obj3)  -> return $ InputUweIO $ objToIO depth . call obj3
                (Right obj3) -> do
                    (obj4l,  obj4r)  <- ignoringConversion objToTuple depth obj3
                    (obj4ll, obj4lr) <- ignoringConversion objToTuple depth obj4l
                    n <- ignoringConversion objToNumber depth obj4ll
                    return $ OutputUweIO n obj4lr $ objToIO depth obj4r
        (Right obj2) -> do
            maybe2 <- ignoringConversion objToMaybe depth obj2
            case maybe2 of
                Nothing     -> return NullUweIO
                (Just obj3) -> do
                    (obj4l, obj4r) <- ignoringConversion objToTuple depth obj3
                    return $ ForkUweIO (objToIO depth obj4l) (objToIO depth obj4r)
