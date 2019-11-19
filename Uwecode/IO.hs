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

data UweIO threadState = UweIO { runIO :: UweController threadState -> StateT threadState IO UweObj }

inputUweIO :: UweObj -> UweIO a
inputUweIO obj = UweIO (\controller -> do
    inp <- giveInput controller
    return $ obj `call` inp)

outputUweIO :: UweObj -> UweObj -> UweIO a
outputUweIO otp nextObj = UweIO (\controller -> do
    controller `takeOutput` otp
    return nextObj)

forkUweIO :: UweObj -> UweIO a -> UweIO a
forkUweIO thisThreadObj newThreadIO = UweIO (\controller -> do
    newState <- newThread controller
    lift $ forkIO $ do
        (_, state) <- runStateT (newThreadIO `runIO` controller) newState
        doneWithProcess controller state
        return ()
    return thisThreadObj)
