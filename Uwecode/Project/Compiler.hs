module Uwecode.Project.Compiler where

import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.CodeReader
import Uwecode.IO
import Uwecode.StdIOs
import Uwecode.Project.Project
import Uwecode.Project.Util
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.IO
import Data.List

makeUweFileHelper :: [(String, String)] -> [String] -> String -> String -> FilePath -> IO FilePath
makeUweFileHelper imports fs close obj = makeFile allImports mainText where
    allImports = imports `union` (makeUnqualifiedImportTups $ map ("Uwecode." ++) ["UweObj", "IO", "BasicUweObjs", "Conversion"])
    mainText = "startRunAndCleanupProcess " ++ showStrList fs ++ " " ++ close ++ " $ objToIO infiniteDepth $ " ++ obj

makeUweFile :: Bool -> FilePath -> MaybeT IO FilePath
makeUweFile isProject path = do
    (projImports, fs, close) <- if isProject then
        return ([("Uwecode.Project.ProjectIOs", "")], ["addIosImportIO", "addIoIO", "setIosCloserIO", "addOptsImportIO", "addOptIO"], "projCloser")
        else lift $ getProjectIOs $ projectLocation path
    mainObj <- getMainObjFromFile path
    (objImports, objString) <- lift $ if isProject then return ([], show mainObj) else optimizeObj (projectLocation path) mainObj
    lift $ makeUweFileHelper (union projImports objImports) fs close objString path

buildUweFile :: Bool -> FilePath -> MaybeT IO FilePath
buildUweFile isProject path = do
    tmpPath <- makeUweFile isProject path
    callGHC $ " -no-keep-hi-files -no-keep-o-files " ++ tmpPath ++ ".hs"
    lift $ rmFile $ tmpPath ++ ".hs"
    return tmpPath

runUweFile :: Bool -> FilePath -> MaybeT IO ()
runUweFile isProject path = do
    tmpPath <- buildUweFile isProject path
    lift $ runFile tmpPath
    lift $ rmFile $ tmpPath
    return ()

printError :: String -> IO ()
printError str = hPutStr stderr (str++"\n")

projCli :: IO ()
projCli = do
    rmFolder ".uwe"
    checkForFail $ runUweFile True "proj.uwe"

cliGivenArgs :: [String] -> IO ()
cliGivenArgs ["run", f]   = checkForFail $ runUweFile False f
cliGivenArgs ["build", f] = checkForFail $ buildUweFile False f
cliGivenArgs ["make", f]  = checkForFail $ makeUweFile False f
cliGivenArgs ["proj"]     = projCli
cliGivenArgs ("run":_)    = printError "Command 'run' takes 1 argument"
cliGivenArgs ("build":_)  = printError "Command 'build' takes 1 argument"
cliGivenArgs ("make":_)   = printError "Command 'make' takes 1 argument"
cliGivenArgs ("proj":_)   = printError "Command 'proj' takes no arguments"
cliGivenArgs []           = printError "No command given"
cliGivenArgs _            = printError "Unrecognized command"
