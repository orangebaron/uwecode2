module Uwecode.IO where

import Uwecode.BasicUweObjs
import Uwecode.Conversion
import Control.Monad.State
import Control.Concurrent
import System.IO

import Uwecode.UweObj

data UweController threadState = UweController {
    giveInput       :: StateT threadState IO UweObj,
    takeOutput      :: UweObj -> StateT threadState IO (),
    newThread       :: StateT threadState IO threadState,
    doneWithProcess :: threadState -> IO ()
}

data UweIO = InputUweIO UweObj | OutputUweIO UweObj UweObj | ForkUweIO UweObj UweObj | NullUweIO

runIO :: UweController threadState -> UweIO -> StateT threadState IO (Maybe UweObj)

runIO controller (InputUweIO obj) = do
    inp <- giveInput controller
    return $ Just $ obj `call` inp

runIO controller (OutputUweIO otp nextObj) = do
    controller `takeOutput` otp
    return $ Just nextObj

runIO controller (ForkUweIO thisThreadObj nextThreadObj) = do
    newState <- newThread controller
    lift $ forkIO $ startAndRunProcess controller newState nextThreadObj
    return $ Just thisThreadObj

runIO controller NullUweIO = return Nothing

runStartedThread :: UweController threadState -> (Maybe UweIO) -> StateT threadState IO ()
runStartedThread controller Nothing = lift $ hPutStr stderr "failed to simplify to expected value"
runStartedThread controller (Just io) = do
    newObj <- runIO controller io
    case newObj of
        (Just obj) -> runStartedThread controller $ objToIO obj
        Nothing -> return ()

startAndRunProcess :: UweController threadState -> threadState -> UweObj -> IO ()
startAndRunProcess controller state obj = do
    (_, newState) <- runStateT (runStartedThread controller $ objToIO obj) state
    doneWithProcess controller newState
    return ()

objToIO :: UweObj -> Maybe UweIO
objToIO obj = case (do
    either1 <- objToEither Nothing obj
    case either1 of
        (Left obj2) -> do
            either2 <- objToEither Nothing obj2
            case either2 of
                (Left obj3) -> return $ InputUweIO obj3
                (Right obj3) -> do
                    (obj4l, obj4r) <- objToTuple Nothing obj3
                    return $ OutputUweIO obj4l obj4r
        (Right obj2) -> do
            maybe2 <- objToMaybe Nothing obj2
            case maybe2 of
                Nothing -> return NullUweIO
                (Just obj3) -> do
                    (obj4l, obj4r) <- objToTuple Nothing obj3
                    return $ ForkUweIO obj4l obj4r) of
    (Left _) -> Nothing
    (Right x) -> Just x
