module Data.Store
(
    savePlan,
    loadPlan
) where

import RIO
import Data.Basic (Plan)
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (readFile, writeFile)
import RIO.Directory (doesFileExist, createDirectoryIfMissing)
import RIO.FilePath (takeDirectory)

savePlan :: Plan -> FilePath -> IO ()
savePlan plan filepath = do
    createDirectoryIfMissing True (takeDirectory filepath)
    writeFile filepath $ encode plan

loadPlan :: FilePath -> IO (Maybe Plan)
loadPlan filepath = do
    exist <- doesFileExist filepath
    if exist
        then readFile filepath <&> decode
        else return Nothing

