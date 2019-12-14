import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.CodeReader
import Uwecode.IO
import Uwecode.StdIOs
import Data.Map
import Control.Monad.State
import Control.Monad.Trans.Maybe
import System.IO
import System.Environment

getProjectIOs :: IO ([UweObj -> UweIOMonad ()], UweIOMonad ())
getProjectIOs = return ([printIO, getThreadNumIO, delayIO], (lift $ putStrLn "thread done")) -- this will be in a Uwecode._ eventually once i add projects

runUweFile :: FilePath -> IO ()
runUweFile path = do
    (fs, close) <- getProjectIOs
    io <- runMaybeT $ getMainIOFromFile path infiniteDepth
    startRunAndCleanupProcess fs close io

printError :: String -> IO ()
printError str = hPutStr stderr (str++"\n")

mainGivenArgs :: [String] -> IO ()
mainGivenArgs ["run", f] = runUweFile f
mainGivenArgs ("run":_)  = printError "Command 'run' takes 1 argument"
mainGivenArgs _          = printError "Unrecognized command"

main = getArgs >>= mainGivenArgs
