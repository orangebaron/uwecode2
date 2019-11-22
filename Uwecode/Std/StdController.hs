module Uwecode.Std.StdController where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Uwecode.Conversion
import Uwecode.IO
import Control.Monad.State
import GHC.Natural
import Data.IORef
import qualified Data.Set as S
import System.IO

type ThreadNum = Natural
data ThreadState = ThreadState { threadNum :: ThreadNum, inpMsg :: UweObj, takenThreads :: IORef (S.Set Natural) }

stdController :: [UweObj -> StateT ThreadState IO ()] -> UweController ThreadState
stdController otpFs = UweController giveInp takeOtp newThr doneWProc where
    giveInp :: StateT ThreadState IO UweObj
    giveInp = do
        state <- get
        return $ inpMsg state

    takeOtp :: UweObj -> StateT ThreadState IO ()
    takeOtp obj = maybe takeOtpUnsuccessful id $ do
        (n, obj2) <- ignoringConversion objToTuple  Nothing obj
        otpFNum   <- ignoringConversion objToNumber Nothing n
        case (drop (naturalToInt otpFNum) otpFs) of
            []    -> Nothing
            (x:_) -> Just (x obj2)

    takeOtpUnsuccessful :: StateT ThreadState IO ()
    takeOtpUnsuccessful = lift $ hPutStr stderr "failed to simplify to expected value\n"

    newThr :: StateT ThreadState IO ThreadState
    newThr = do
        n      <- firstUntakenThread
        state  <- get
        put    $ ThreadState (threadNum state) (churchNum n) (takenThreads state)
        return $ ThreadState n (churchNum $ threadNum state) (takenThreads state)

    firstUntakenThread :: StateT ThreadState IO ThreadNum
    firstUntakenThread = do
        state <- get
        lift $ atomicModifyIORef (takenThreads state) (firstUntakenThreadHelper 0)

    firstUntakenThreadHelper :: ThreadNum -> S.Set Natural -> (S.Set Natural, Natural)
    firstUntakenThreadHelper n set
        | S.member n set = firstUntakenThreadHelper (n+1) set
        | otherwise      = (S.insert n set, n)

    doneWProc :: ThreadState -> IO ()
    doneWProc state = modifyIORef (takenThreads state) (S.delete $ threadNum state)

newThreadState :: IO ThreadState
newThreadState = do
    ref <- newIORef $ S.singleton 0
    return $ ThreadState 0 (churchNum 0) ref
