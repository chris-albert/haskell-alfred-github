{-# LANGUAGE DeriveDataTypeable #-}
module Args where

import System.Console.CmdArgs
import Control.Lens

data ArgsMode = 
  Store 
  {
    configFile :: FilePath
  }
  | 
  Get 
  {
    configFile :: FilePath
  } 
  deriving(Data,Show,Typeable)

store :: ArgsMode
store = Store 
  {
    configFile = def &= typFile
  }

get :: ArgsMode
get = Get
  {
    configFile = def &= typFile
  }

argsMode :: Mode (CmdArgs ArgsMode)
argsMode = cmdArgsMode $ modes [get,store]
  &= summary "haskell-alfred-github v0.0.1, Chris Albert"

getArgs :: IO ArgsMode
getArgs = cmdArgsRun argsMode
