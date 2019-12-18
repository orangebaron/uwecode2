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
import System.Directory
import Data.Maybe

maybeToMaybeT :: (Monad m) => Maybe a -> MaybeT m a
maybeToMaybeT = MaybeT . return

data FileContext = FileContext { thisFile :: FilePath, importedPath :: Maybe FileContext }

checkForCircularDependency :: FilePath -> FileContext -> Bool
checkForCircularDependency path context = (path /= thisFile context) && (maybe True (checkForCircularDependency path) $ importedPath context)

readDeclarationAST :: FileContext -> GlobalVarMap -> DeclarationAST -> MaybeT IO GlobalVarMap
readDeclarationAST _ map (Equals isPublic var exp) = maybeToMaybeT $ do
    obj <- readExpressionAST (VarMap M.empty map) exp
    newMap <- setGlobalVar map isPublic var obj
    return newMap

readDeclarationAST context oldVars (Import file prefix just) = do
    filePath <- lift $ canonicalizePath $ takeDirectory (thisFile context) </> file
    guard $ checkForCircularDependency filePath context
    (GlobalVarMap _ importedVars) <- getDictFromFile $ FileContext filePath $ Just context
    maybeToMaybeT $ foldl (>>=) (return oldVars) $ map (\var vars -> do
        obj <- importedVars M.!? var
        setGlobalVar vars False (prefix++var) obj) $
            if (null just) then (M.keys importedVars) else just


readCodeAST :: FileContext -> GlobalVarMap -> CodeAST -> MaybeT IO GlobalVarMap
readCodeAST _ map [] = maybeToMaybeT $ Just map
readCodeAST file map (a:b) = do
    newMap <- readDeclarationAST file map a
    readCodeAST file newMap b

code :: Parser CodeAST
code = listed declaration

varMap :: FileContext -> Parser (MaybeT IO GlobalVarMap)
varMap context = do
    codeAST <- code
    return $ readCodeAST context emptyGlobalVarMap codeAST

readUweString :: FileContext -> String -> MaybeT IO GlobalVarMap
readUweString context str = maybe (maybeToMaybeT Nothing) id $ varMap context `takeFirstParse` str

getDictFromFile :: FileContext -> MaybeT IO GlobalVarMap
getDictFromFile context = do
    str <- lift $ readFile $ thisFile context
    readUweString context str

getMainObjFromFile :: FilePath -> MaybeT IO UweObj
getMainObjFromFile file = do
    dict <- getDictFromFile $ FileContext file Nothing
    maybeToMaybeT $ getGlobalVar dict "main"

getMainIOFromFile :: FilePath -> Depth -> MaybeT IO UweIO
getMainIOFromFile file depth = do
    main <- getMainObjFromFile file
    maybeToMaybeT $ objToIO depth main
