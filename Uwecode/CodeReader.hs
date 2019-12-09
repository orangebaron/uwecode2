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
import System.FilePath

maybeToMaybeT :: (Monad m) => Maybe a -> MaybeT m a
maybeToMaybeT = MaybeT . return

readDeclarationAST :: FilePath -> GlobalVarMap -> DeclarationAST -> MaybeT IO GlobalVarMap
readDeclarationAST _ map (Equals isPublic var exp) = maybeToMaybeT $ do
    obj <- readExpressionAST (VarMap M.empty map) exp
    newMap <- setGlobalVar map isPublic var obj
    return newMap

readDeclarationAST currentFile oldVars (Import file prefix just) = do
        (GlobalVarMap _ importedVars) <- getDictFromFile $ takeDirectory currentFile ++ "/" ++ file
        maybeToMaybeT $ foldl (>>=) (return oldVars) $ map (\var vars -> do
                obj <- importedVars M.!? var
                setGlobalVar vars False (prefix++var) obj) $
                        if (null just) then (M.keys importedVars) else just

readCodeAST :: FilePath -> GlobalVarMap -> CodeAST -> MaybeT IO GlobalVarMap
readCodeAST _ map [] = maybeToMaybeT $ Just map
readCodeAST file map (a:b) = do
    newMap <- readDeclarationAST file map a
    readCodeAST file newMap b

code :: Parser CodeAST
code = listed declaration

varMap :: FilePath -> Parser (MaybeT IO GlobalVarMap)
varMap file = do
    codeAST <- code
    return $ readCodeAST file emptyGlobalVarMap codeAST

readUweString :: FilePath -> String -> MaybeT IO GlobalVarMap
readUweString file str = maybe (maybeToMaybeT Nothing) id $ varMap file `takeFirstParse` str

getDictFromFile :: FilePath -> MaybeT IO GlobalVarMap
getDictFromFile file = do
    str <- lift $ readFile file
    readUweString file str

getMainObjFromFile :: FilePath -> MaybeT IO UweObj
getMainObjFromFile file = do
    dict <- getDictFromFile file
    maybeToMaybeT $ getGlobalVar dict "main"

getMainIOFromFile :: FilePath -> Depth -> MaybeT IO UweIO
getMainIOFromFile file depth = do
    main <- getMainObjFromFile file
    maybeToMaybeT $ objToIO depth main
