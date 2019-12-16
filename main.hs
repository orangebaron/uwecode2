import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.CodeReader
import Uwecode.IO
import Uwecode.StdIOs
import Uwecode.Project
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.IO
import System.Environment
import System.Process
import System.Directory
import Data.Maybe
import Data.List
import System.FilePath

showStrList :: [String] -> String
showStrList s = '[' : (concat $ intersperse ", " s) ++ "]"

makeUweFileText :: [String] -> [String] -> String -> UweObj -> String
makeUweFileText imports fs close obj = importsStr ++ "\nmain = startRunAndCleanupProcess " ++ showStrList fs ++ " " ++ close ++ " $ objToIO infiniteDepth $ " ++ show obj ++ "\n" where
    importsStr = concat $ map (("import " ++) . (++ "\n")) allImports
    allImports = imports ++ ["Uwecode.UweObj", "Uwecode.IO", "Uwecode.BasicUweObjs", "Uwecode.Conversion"]

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
    (imports, fs, close) <- lift $ getProjectIOs
    io <- getMainObjFromFile path
    fileName <- lift $ figureOutGoodFileName $ takeFileName $ dropExtension $ path
    lift $ writeFile (fileName ++ ".hs") $ makeUweFileText imports fs close io
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

mainGivenArgs :: [String] -> IO ()
mainGivenArgs ["run", f]   = checkForFail $ runUweFile f
mainGivenArgs ["build", f] = checkForFail $ buildUweFile f
mainGivenArgs ("run":_)    = printError "Command 'run' takes 1 argument"
mainGivenArgs ("build":_)  = printError "Command 'build' takes 1 argument"
mainGivenArgs []           = printError "No command given"
mainGivenArgs _            = printError "Unrecognized command"

main = getArgs >>= mainGivenArgs
