module Uwecode.ReadFile where

import Uwecode.UweObj
import Uwecode.AST
import Uwecode.IO
import Uwecode.Conversion
import Uwecode.Parser.CodeReader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Map

maybeToMaybeT :: (Monad m) => Maybe a -> MaybeT m a
maybeToMaybeT = MaybeT . return

getDictFromFile :: FilePath -> MaybeT IO GlobalVarMap
getDictFromFile file = do
    str <- lift $ readFile file
    maybeToMaybeT $ readUweString str

getMainObjFromFile :: FilePath -> MaybeT IO UweObj
getMainObjFromFile file = do
    dict <- getDictFromFile file
    maybeToMaybeT $ dict !? "main"

getMainIOFromFile :: FilePath -> Depth -> MaybeT IO UweIO
getMainIOFromFile file depth = do
    main <- getMainObjFromFile file
    maybeToMaybeT $ objToIO depth main
