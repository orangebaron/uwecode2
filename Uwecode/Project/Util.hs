module Uwecode.Project.Util where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.IO
import System.Process
import System.Directory
import Data.Maybe
import Data.List

showStrList :: [String] -> String
showStrList s = '[' : (concat $ intersperse ", " s) ++ "]"

makeImportStatements :: [(String, String)] -> String
makeImportStatements = concat . map helper where
    helper (i, "") = "import " ++ i ++ "\n"
    helper (i, j)  = "import qualified " ++ i ++ " as " ++ j ++ "\n"

figureOutGoodFileName :: FilePath -> IO FilePath
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

makeFileText :: [(String, String)] -> String -> String
makeFileText imports mainText = makeImportStatements imports ++ "\nmain = " ++ mainText ++ "\n"

makeFile :: [(String, String)] -> String -> FilePath -> IO FilePath
makeFile imports mainText startPath = do
    path <- figureOutGoodFileName startPath
    writeFile (path ++ ".hs") $ makeFileText imports mainText
    return path

findGHC :: MaybeT IO FilePath
findGHC = ioMaybeToMaybeTIO $ findExecutable "ghc"

callGHC :: String -> MaybeT IO ()
callGHC args = do
    ghc <- findGHC
    lift $ system $ ghc ++ args -- TODO look at exit code
    return ()

rmFile :: FilePath -> IO ()
rmFile path = (system $ "rm " ++ path) >> return ()

rmFolder :: FilePath -> IO ()
rmFolder path = (system $ "rm -r " ++ path) >> return ()

runFile :: FilePath -> IO ()
runFile path = (system $ "./" ++ path) >> return ()
