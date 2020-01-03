module Uwecode.Project.ProjectIOs where

import System.IO
import Uwecode.Project.Project

readIosFile :: IO ([(String, String)], String)
readIosFile = do
    text <- readFile $ iosLocation "."
    return $ if text == "" then ([], "") else read text

writeIosFile :: ([(String, String)], String) -> IO ()
writeIosFile contents = writeFile (iosLocation ".") $ show contents

addIosImport :: (String, String) -> IO ()
addIosImport imp = do
    (imps, closer) <- readIosFile
    writeIosFile (imp : imps, closer)

setIosCloser :: String -> IO ()
setIosCloser closer = do
    (imps, _) <- readIosFile
    writeIosFile (imps, closer)
