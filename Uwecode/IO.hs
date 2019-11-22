module Uwecode.IO where

import Uwecode.UweObj
import Uwecode.BasicUweObjs
import Control.Monad.State
import Control.Concurrent
import System.IO
import Data.IORef
import GHC.Natural
import qualified Data.Set as S

type ThreadNum = Natural
data ThreadState = ThreadState { threadNum :: ThreadNum, inpMsg :: UweObj, takenThreads :: IORef (S.Set Natural) }

data UweIO = InputUweIO (UweObj -> (Maybe UweIO)) | OutputUweIO Natural UweObj (Maybe UweIO) | ForkUweIO (Maybe UweIO) (Maybe UweIO) | NullUweIO

type UweIOMonad x = StateT ThreadState IO x

runIO :: [UweObj -> UweIOMonad ()] -> UweIOMonad () -> UweIO -> UweIOMonad ()

runIO fs close (InputUweIO next) = do
    state <- get
    runMaybeIO fs close $ next $ inpMsg state

runIO fs close (OutputUweIO n otp next) = case (drop (naturalToInt n) fs) of
    [] -> unsuccessful
    (x:_) -> do
        x otp
        runMaybeIO fs close next

runIO fs close (ForkUweIO next other) = do
    n      <- firstUntakenThread
    state  <- get
    put    $ ThreadState (threadNum state) (churchNum n) (takenThreads state)
    lift   $ forkIO $ do
        runStateT (runMaybeIO fs close other) $ ThreadState n (churchNum $ threadNum state) (takenThreads state)
        return ()
    runMaybeIO fs close next
    runMaybeIO fs close next

runIO fs close NullUweIO = do
    close
    state <- get
    lift  $ modifyIORef (takenThreads state) (S.delete $ threadNum state)

firstUntakenThread :: StateT ThreadState IO ThreadNum
firstUntakenThread = do
    state <- get
    lift  $ atomicModifyIORef (takenThreads state) (firstUntakenThreadHelper 0)

firstUntakenThreadHelper :: ThreadNum -> S.Set Natural -> (S.Set Natural, Natural)
firstUntakenThreadHelper n set
    | S.member n set = firstUntakenThreadHelper (n+1) set
    | otherwise      = (S.insert n set, n)

unsuccessful :: UweIOMonad ()
unsuccessful = lift $ hPutStr stderr "failed to simplify to expected value\n"

runMaybeIO :: [UweObj -> UweIOMonad ()] -> UweIOMonad () -> Maybe UweIO -> UweIOMonad ()
runMaybeIO fs close = maybe unsuccessful (runIO fs close)
