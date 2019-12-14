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
import System.Cmd
import System.Directory
import Data.Maybe
import Data.List

showStrList :: [String] -> String
showStrList s = '[' : (concat $ intersperse ", " s) ++ "]"

makeUweFileText :: [String] -> [String] -> String -> UweObj -> String
makeUweFileText imports fs close obj = importsStr ++ "\nmain = startRunAndCleanupProcess " ++ showStrList fs ++ " " ++ close ++ " $ objToIO infiniteDepth $ " ++ show obj ++ "\n" where
    importsStr = concat $ map (("import " ++) . (++ "\n")) allImports
    allImports = imports ++ ["Uwecode.UweObj", "Uwecode.IO", "Uwecode.BasicUweObjs", "Uwecode.Conversion"]

figureOutGoodFileName :: IO String
figureOutGoodFileName = helper "tmp" where
    helper :: String -> IO String
    helper s = do
        f1 <- findFile ["."] s
        f2 <- findFile ["."] (s++".hs")
        if isNothing f1 && isNothing f2 then return s else helper $ '_':s

ioMaybeToMaybeTIO :: IO (Maybe a) -> MaybeT IO a
ioMaybeToMaybeTIO io = do
    m <- lift io
    MaybeT $ return m

checkForFail :: MaybeT IO () -> IO ()
checkForFail mio = do
    m <- runMaybeT mio
    if isNothing m then hPutStr stderr "failed\n" else return ()

makeUweFile :: FilePath -> MaybeT IO String
makeUweFile path = do
    (imports, fs, close) <- lift $ getProjectIOs
    io <- getMainObjFromFile path
    fileName <- lift $ figureOutGoodFileName
    lift $ writeFile (fileName ++ ".hs") $ makeUweFileText imports fs close io
    return fileName

runUweFile :: FilePath -> MaybeT IO ()
runUweFile path = do
    tmpPath <- makeUweFile path
    ghc <- ioMaybeToMaybeTIO $ findExecutable "ghc"
    lift $ system $ ghc ++ " " ++ tmpPath ++ ".hs"
    lift $ system $ "rm " ++ (tmpPath ++ ".hs")
    -- TODO break this into a different function or something
    -- TODO remove .hi and .o
    lift $ system $ "./" ++ tmpPath
    lift $ system $ "rm " ++ tmpPath
    return ()

printError :: String -> IO ()
printError str = hPutStr stderr (str++"\n")

mainGivenArgs :: [String] -> IO ()
mainGivenArgs ["run", f] = checkForFail $ runUweFile f
mainGivenArgs ("run":_)  = printError "Command 'run' takes 1 argument"
mainGivenArgs _          = printError "Unrecognized command"

main = getArgs >>= mainGivenArgs
