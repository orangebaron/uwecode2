module Uwecode.Project.Project where

import Uwecode.UweObj
import Uwecode.IO
import Uwecode.StdIOs
import Uwecode.Project.Util
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import System.IO
import System.FilePath
import Data.List
import System.Process
import qualified Data.Set

projectFolderName :: FilePath
projectFolderName = ".uwe"

projectLocation :: FilePath -> FilePath
projectLocation f = takeDirectory f </> projectFolderName

iosFileName :: FilePath
iosFileName = "ios"

optsFileName :: FilePath
optsFileName = "opts"

iosLocation :: FilePath -> FilePath
iosLocation projLoc = projLoc </> iosFileName

optsLocation :: FilePath -> FilePath
optsLocation projLoc = projLoc </> optsFileName

makeUnqualifiedImportTups :: [String] -> [(String, String)]
makeUnqualifiedImportTups = map (\x -> (x, ""))

getProjectIOs :: FilePath -> IO ([(String, String)], [String], String)
getProjectIOs proj = do
    ios <- readFile $ iosLocation proj
    return $ read ios
    -- TODO error checking instead of just crash

getProjectOpts :: FilePath -> IO ([(String, String)], [String])
getProjectOpts proj = do
    opts <- readFile $ optsLocation proj
    return $ read opts
    -- TODO error checking instead of just crash

makeOptimizeObjFile :: FilePath -> Depth -> [(String, String)] -> [String] -> UweObj -> IO FilePath
makeOptimizeObjFile path depth imports opts obj = makeFile allImports mainText path where
    allImports = imports `union` (makeUnqualifiedImportTups $ ["Data.Set", "Control.Monad.State"] ++ (map ("Uwecode." ++) ["UweObj", "BasicUweObjs", "Project.Util", "Project.Optimize"]))
    mainText = "print $ runState (makeStringConversion " ++ showStrList opts ++ " (" ++ show depth ++ ") $ " ++ show obj ++ ") $ empty"

optimizeObj :: FilePath -> UweObj -> IO ([(String, String)], String)
optimizeObj proj obj = do
    (imports, opts) <- getProjectOpts proj
    path <- makeOptimizeObjFile "opt" infiniteDepth imports opts obj
    runMaybeT $ callGHC $ " " ++ path ++ ".hs"
    otpStr <- readProcess ("./" ++ path) [] [] -- TODO verrrrry ugly
    rmFile path
    rmFile (path ++ ".hs")
    let (str, set) = read otpStr in return (Data.Set.toList set, str)
