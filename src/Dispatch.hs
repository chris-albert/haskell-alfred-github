module Dispatch where

import qualified GitConfig as GC
import qualified Git as G
import qualified Args

dispatch :: Args.ArgsMode -> GC.GitConfig -> IO String
dispatch (Args.Store _) = storeResults
dispatch (Args.Get _)   = getCache

storeResults :: GC.GitConfig -> IO String
storeResults config = do
  (json, count) <- G.getReposJSON config
  _             <- writeFile (GC.cacheFile config) json
  return $ "Updated " ++ (show count) ++ " repos successfully"

getCache :: GC.GitConfig -> IO String
getCache config = readFile (GC.cacheFile config)
