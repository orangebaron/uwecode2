module Uwecode.CodeReader where

import Uwecode.UweObj
import Uwecode.AST
import Uwecode.IO
import Uwecode.Conversion
import Uwecode.Parser.Parser
import Uwecode.Parser.DeclarationParsers
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.Map as M

maybeToMaybeT :: (Monad m) => Maybe a -> MaybeT m a
maybeToMaybeT = MaybeT . return

readDeclarationAST :: GlobalVarMap -> DeclarationAST -> MaybeT IO GlobalVarMap
readDeclarationAST map (Equals isPublic var exp) = maybeToMaybeT $ do
    obj <- readExpressionAST (VarMap M.empty map) exp
    newMap <- setGlobalVar map isPublic var obj
    return newMap

readDeclarationAST oldVars (Import file prefix just) = do
        (GlobalVarMap _ importedVars) <- getDictFromFile file
        maybeToMaybeT $ foldl (>>=) (return oldVars) $ map (\var vars -> do
                obj <- importedVars M.!? var
                setGlobalVar vars False (prefix++var) obj) $
                        if (null just) then (M.keys importedVars) else just

readCodeAST :: GlobalVarMap -> CodeAST -> MaybeT IO GlobalVarMap
readCodeAST map [] = maybeToMaybeT $ Just map
readCodeAST map (a:b) = do
    newMap <- readDeclarationAST map a
    readCodeAST newMap b

code :: Parser CodeAST
code = listed declaration

varMap :: Parser (MaybeT IO GlobalVarMap)
varMap = do
    codeAST <- code
    return $ readCodeAST emptyGlobalVarMap codeAST

readUweString :: String -> MaybeT IO GlobalVarMap
readUweString str = maybe (maybeToMaybeT Nothing) id $ varMap `takeFirstParse` str

getDictFromFile :: FilePath -> MaybeT IO GlobalVarMap
getDictFromFile file = do
    str <- lift $ readFile file
    readUweString str

getMainObjFromFile :: FilePath -> MaybeT IO UweObj
getMainObjFromFile file = do
    dict <- getDictFromFile file
    maybeToMaybeT $ getGlobalVar dict "main"

getMainIOFromFile :: FilePath -> Depth -> MaybeT IO UweIO
getMainIOFromFile file depth = do
    main <- getMainObjFromFile file
    maybeToMaybeT $ objToIO depth main
