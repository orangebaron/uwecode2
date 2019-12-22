module Uwecode.Project.Optimize where

import Uwecode.UweObj
import Control.Monad.State
import Data.Set
import Data.Maybe

type Conversion = (UweObj -> State (Set String) String) -> Depth -> UweObj -> Maybe (State (Set String) String)

convertObjs :: [Conversion] -> Depth -> UweObj -> State (Set String) String

makeStringConversion :: [Conversion] -> UweStringConversion
makeStringConversion convs depth obj = helper convs (Just 0) where
    helper :: [Conversion] -> Depth -> Maybe (State (Set String) String)
    helper [] d
        | d == depth = Just $ return $ show obj
        | otherwise = helper convs $ incrementDepth d
    helper (conv:rest) d = maybe (helper rest d) id $ conv (makeStringConversion convs) d obj
