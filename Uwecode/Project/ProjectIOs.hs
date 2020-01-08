module Uwecode.Project.ProjectIOs where

import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.IO
import Uwecode.Project.Project
import System.IO
import Control.Monad.State

readIosFile :: IO ([(String, String)], String)
readIosFile = do
    text <- readFile $ iosLocation "."
    return $ if text == "" then ([], "") else read text

writeIosFile :: ([(String, String)], String) -> IO ()
writeIosFile contents = writeFile (iosLocation ".") $ show contents

addIosImport :: (String, String) -> IO ()
addIosImport imp = do
    (imps, closer) <- readIosFile
    writeIosFile (imp : imps, closer)

setIosCloser :: String -> IO ()
setIosCloser closer = do
    (imps, _) <- readIosFile
    writeIosFile (imps, closer)

addIosImportIO :: UweObj -> UweIOMonad ()
addIosImportIO obj = maybe unsuccessful (lift . addIosImport) $ do
    (leftObj, rightObj) <- ignoringConversion objToTuple Nothing obj
    left <- ignoringConversion objToString Nothing leftObj
    right <- ignoringConversion objToString Nothing rightObj
    return (left, right)

setIosCloserIO :: UweObj -> UweIOMonad ()
setIosCloserIO = maybe unsuccessful (lift . setIosCloser) . ignoringConversion objToString Nothing

readOptsFile :: IO ([(String, String)], [String])
readOptsFile = do
    text <- readFile $ optsLocation "."
    return $ if text == "" then ([], []) else read text

writeOptsFile :: ([(String, String)], [String]) -> IO ()
writeOptsFile contents = writeFile (optsLocation ".") $ show contents

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
projCloser = lift $ print "done" -- TODO default values in file
