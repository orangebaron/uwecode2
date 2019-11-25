import Uwecode.UweObj
import Uwecode.Conversion
import Uwecode.Parser.CodeReader
import Uwecode.IO
import Uwecode.StdIOs
import Data.Map
import Control.Monad.State

readCode :: String -> Maybe UweIO
readCode str = readUweString str >>= (!? "main") >>= (objToIO Nothing)

main = do
    str <- readFile "main.uwe"
    startRunAndCleanupProcess [printIO, getThreadNumIO, delayIO] (lift $ putStrLn "thread done") $ readCode str
