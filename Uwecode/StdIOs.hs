module Uwecode.StdIOs where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Uwecode.Conversion
import Uwecode.IO
import Control.Monad.State
import Control.Time

getThreadNum :: UweIOMonad ThreadNum
getThreadNum = do
    state <- get
    return $ threadNum state

setInpMsg :: UweObj -> UweIOMonad ()
setInpMsg msg = do
    state <- get
    put $ ThreadState (threadNum state) msg (takenThreads state)

printIO :: UweObj -> UweIOMonad ()
printIO = maybe unsuccessful (lift . putStrLn) . ignoringConversion objToString Nothing

getThreadNumIO :: a -> UweIOMonad ()
getThreadNumIO = const $ do
    n <- getThreadNum
    setInpMsg $ churchNum n

delayIO :: UweObj -> UweIOMonad ()
delayIO = maybe unsuccessful (lift . delay . (* (1e-6 :: Double)) . fromIntegral) . ignoringConversion objToNumber Nothing
