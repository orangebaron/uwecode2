module Uwecode.Compiler where

import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.CodeReader
import Uwecode.IO
import Uwecode.StdIOs
import Uwecode.Project
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.IO
import System.Process
import System.Directory
import Data.Maybe
import Data.List
import System.FilePath

showStrList :: [String] -> String
showStrList s = '[' : (concat $ intersperse ", " s) ++ "]"

makeImportStatements :: [(String, String)] -> String
makeImportStatements = concat . map helper where
    helper (i, "") = "import " ++ i ++ "\n"
    helper (i, j)  = "import qualified " ++ i ++ " as " ++ j ++ "\n"

makeUweFileText :: [(String, String)] -> [String] -> String -> String -> String
makeUweFileText imports fs close obj = importsStr ++ "\nmain = startRunAndCleanupProcess " ++ showStrList fs ++ " " ++ close ++ " $ objToIO infiniteDepth $ " ++ obj ++ "\n" where
    importsStr = makeImportStatements allImports
    allImports = imports ++ (makeUnqualifiedImportTups $ map ("Uwecode." ++) ["UweObj", "IO", "BasicUweObjs", "Conversion"])

figureOutGoodFileName :: String -> IO String
figureOutGoodFileName s = do
    f1 <- findFile ["."] s
    f2 <- findFile ["."] (s++".hs")
    if isNothing f1 && isNothing f2 then return s else figureOutGoodFileName $ '_':s

ioMaybeToMaybeTIO :: IO (Maybe a) -> MaybeT IO a
ioMaybeToMaybeTIO io = do
    m <- lift io
    MaybeT $ return m

checkForFail :: MaybeT IO a -> IO ()
checkForFail mio = do
    m <- runMaybeT mio
    if isNothing m then hPutStr stderr "failed\n" else return ()

makeUweFile :: FilePath -> MaybeT IO String
makeUweFile path = do
    (projImports, fs, close) <- lift $ getProjectIOs $ projectLocation path
    mainObj <- getMainObjFromFile path
    (objImports, objString) <- lift $ optimizeObj (projectLocation path) mainObj
    fileName <- lift $ figureOutGoodFileName $ takeFileName $ dropExtension $ path
    lift $ writeFile (fileName ++ ".hs") $ makeUweFileText (union projImports objImports) fs close objString
    return fileName

buildUweFile :: FilePath -> MaybeT IO FilePath
buildUweFile path = do
    tmpPath <- makeUweFile path
    ghc <- ioMaybeToMaybeTIO $ findExecutable "ghc"
    lift $ system $ ghc ++ " -no-keep-hi-files -no-keep-o-files " ++ tmpPath ++ ".hs" -- TODO look at exit code
    lift $ system $ "rm " ++ (tmpPath ++ ".hs")
    return tmpPath

runUweFile :: FilePath -> MaybeT IO ()
runUweFile path = do
    tmpPath <- buildUweFile path
    lift $ system $ "./" ++ tmpPath
    lift $ system $ "rm " ++ tmpPath
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