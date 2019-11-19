module Uwecode.IO where

import Control.Monad.State
import Control.Concurrent

import Uwecode.UweObj

data UweController threadState = UweController {
    giveInput       :: StateT threadState IO UweObj,
    takeOutput      :: UweObj -> StateT threadState IO (),
    newThread       :: StateT threadState IO threadState,
    doneWithProcess :: threadState -> IO ()
}

data UweIO = InputUweIO UweObj | OutputUweIO UweObj UweObj | ForkUweIO UweObj UweIO

runIO :: UweController threadState -> UweIO -> StateT threadState IO UweObj

runIO controller (InputUweIO obj) = do
    inp <- giveInput controller
    return $ obj `call` inp

runIO controller (OutputUweIO otp nextObj) = do
    controller `takeOutput` otp
    return nextObj

runIO controller (ForkUweIO thisThreadObj newThreadIO) = do
    newState <- newThread controller
    lift $ forkIO $ do
        (_, state) <- runStateT (runIO controller newThreadIO) newState
        doneWithProcess controller state
        return ()
    return thisThreadObj
