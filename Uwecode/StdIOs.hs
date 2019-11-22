module Uwecode.StdIOs where

import Uwecode.UweObj
import Uwecode.IO
import Control.Monad.State

printIO :: UweObj -> UweIOMonad ()
printIO = lift . print
