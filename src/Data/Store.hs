module Data.Store
(
    savePlan,
    loadPlan
) where

import Prelude hiding (readFile, writeFile)
import Data.Basic (Plan)
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (readFile, writeFile)

savePlan :: Plan -> FilePath -> IO ()
savePlan plan filepath = writeFile filepath $ encode plan

loadPlan :: FilePath -> IO (Maybe Plan)
loadPlan filepath = do
    content <- readFile filepath
    return $ decode content

