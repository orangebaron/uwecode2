module Uwecode.Project.ProjectIOs where

import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.IO
import Uwecode.Project.Project
import System.IO
import Control.Monad.State
import Control.Exception

defltIos = ([], [], "")
defltOpts = ([], [])
tryRead :: IO String -> IO (Either IOError String)
tryRead = try

readIosFile :: IO ([(String, String)], [String], String)
readIosFile = do
    eitherText <- tryRead $ readFile $ iosLocation $ projectLocation "."
    return $ let text = either (const "") id eitherText in (if text == "" then defltIos else read text)

writeIosFile :: ([(String, String)], [String], String) -> IO ()
writeIosFile contents = writeFile (iosLocation $ projectLocation ".") $ show contents ++ "\n"

addIosImport :: (String, String) -> IO ()
addIosImport imp = do
    (imps, ios, closer) <- readIosFile
    writeIosFile (imp : imps, ios, closer)

addIo :: String -> IO ()
addIo io = do
    (imps, ios, closer) <- readIosFile
    writeIosFile (imps, io : ios, closer)

setIosCloser :: String -> IO ()
setIosCloser closer = do
    (imps, ios, _) <- readIosFile
    writeIosFile (imps, ios, closer)

addIosImportIO :: UweObj -> UweIOMonad ()
addIosImportIO obj = maybe unsuccessful (lift . addIosImport) $ do
    (leftObj, rightObj) <- ignoringConversion objToTuple Nothing obj
    left <- ignoringConversion objToString Nothing leftObj
    right <- ignoringConversion objToString Nothing rightObj
    return (left, right)

addIoIO :: UweObj -> UweIOMonad ()
addIoIO = maybe unsuccessful (lift . addIo) . ignoringConversion objToString Nothing

setIosCloserIO :: UweObj -> UweIOMonad ()
setIosCloserIO = maybe unsuccessful (lift . setIosCloser) . ignoringConversion objToString Nothing

readOptsFile :: IO ([(String, String)], [String])
readOptsFile = do
    eitherText <- tryRead $ readFile $ optsLocation $ projectLocation "."
    return $ let text = either (const "") id eitherText in (if text == "" then defltOpts else read text)

writeOptsFile :: ([(String, String)], [String]) -> IO ()
writeOptsFile contents = writeFile (optsLocation $ projectLocation ".") $ show contents ++ "\n"

addOptsImport :: (String, String) -> IO ()
addOptsImport imp = do
    (imps, opts) <- readOptsFile
    writeOptsFile (imp : imps, opts)

addOpt :: String -> IO ()
addOpt opt = do
    (imps, opts) <- readOptsFile
    writeOptsFile (imps, opt : opts)

addOptsImportIO :: UweObj -> UweIOMonad ()
addOptsImportIO obj = maybe unsuccessful (lift . addOptsImport) $ do
    (leftObj, rightObj) <- ignoringConversion objToTuple Nothing obj
    left <- ignoringConversion objToString Nothing leftObj
    right <- ignoringConversion objToString Nothing rightObj
    return (left, right)

addOptIO :: UweObj -> UweIOMonad ()
addOptIO = maybe unsuccessful (lift . addOpt) .  ignoringConversion objToString Nothing

projCloser :: UweIOMonad ()
projCloser = lift $ do
    ios <- readIosFile
    if ios == defltIos then writeIosFile defltIos else return ()
    opts <- readOptsFile
    if opts == defltOpts then writeOptsFile defltOpts else return ()
