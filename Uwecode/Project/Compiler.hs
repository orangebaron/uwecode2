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
    allImports = imports ++ (makeUnqualifiedImportTups $ map ("Uwecode." ++) ["UweObj", "IO", "BasicUweObjs", "Conversion"])
    mainText = "startRunAndCleanupProcess " ++ showStrList fs ++ " " ++ close ++ " $ objToIO infiniteDepth $ " ++ obj

makeUweFile :: FilePath -> MaybeT IO FilePath
makeUweFile path = do
    (projImports, fs, close) <- lift $ getProjectIOs $ projectLocation path
    mainObj <- getMainObjFromFile path
    (objImports, objString) <- lift $ optimizeObj (projectLocation path) mainObj
    lift $ makeUweFileHelper (union projImports objImports) fs close objString path

buildUweFile :: FilePath -> MaybeT IO FilePath
buildUweFile path = do
    tmpPath <- makeUweFile path
    callGHC $ " -no-keep-hi-files -no-keep-o-files " ++ tmpPath ++ ".hs"
    lift $ rmFile $ tmpPath ++ ".hs"
    return tmpPath

runUweFile :: FilePath -> MaybeT IO ()
runUweFile path = do
    tmpPath <- buildUweFile path
    lift $ runFile tmpPath
    lift $ rmFile $ tmpPath
    return ()

printError :: String -> IO ()
printError str = hPutStr stderr (str++"\n")

cliGivenArgs :: [String] -> IO ()
cliGivenArgs ["run", f]   = checkForFail $ runUweFile f
cliGivenArgs ["build", f] = checkForFail $ buildUweFile f
cliGivenArgs ["make", f]  = checkForFail $ makeUweFile f
cliGivenArgs ("run":_)    = printError "Command 'run' takes 1 argument"
cliGivenArgs ("build":_)  = printError "Command 'build' takes 1 argument"
cliGivenArgs ("make":_)   = printError "Command 'make' takes 1 argument"
cliGivenArgs []           = printError "No command given"
cliGivenArgs _            = printError "Unrecognized command"
