import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.Parser.CodeReader
import Uwecode.IO
import Uwecode.StdIOs
import Uwecode.ReadFile
import Data.Map
import Control.Monad.State
import Control.Monad.Trans.Maybe

main = do
    io <- runMaybeT $ getMainIOFromFile "main.uwe" Nothing
    startRunAndCleanupProcess [printIO, getThreadNumIO, delayIO] (lift $ putStrLn "thread done") io
