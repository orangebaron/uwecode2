module Uwecode.Project where

import Uwecode.UweObj
import Uwecode.IO
import Uwecode.StdIOs
import Control.Monad.Trans.Class
import System.IO

getProjectIOs :: IO ([String], [String], String)
getProjectIOs = return (["Uwecode.StdIOs", "Control.Monad.Trans.Class"], ["printIO", "getThreadNumIO", "delayIO"], "(lift $ putStrLn \"thread done\\n\")") --TODO
