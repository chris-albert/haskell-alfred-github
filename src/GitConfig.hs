module GitConfig where

import qualified Data.ConfigFile as CF
import Control.Monad.Except

data GitConfig = GitConfig {
  protocol  :: String,
  host      :: String,
  token     :: String,
  cacheFile :: String
} deriving Show

readConfig :: String -> IO GitConfig

readConfig fileName =
  do
    ioConfig <- runExceptT $
      do
       cp <- join $ liftIO $ CF.readfile CF.emptyCP fileName
       p  <- CF.get cp "DEFAULT" "protocol"  :: ExceptT CF.CPError IO String
       h  <- CF.get cp "DEFAULT" "host"      :: ExceptT CF.CPError IO String
       t  <- CF.get cp "DEFAULT" "token"     :: ExceptT CF.CPError IO String
       f  <- CF.get cp "DEFAULT" "cachefile" :: ExceptT CF.CPError IO String
       let jc = GitConfig p h t f
       return jc
    eitherIO ioConfig

eitherIO :: (Show e) => Either e a -> IO a
eitherIO (Left e) = error $ show e
eitherIO (Right a) = return a

buildUrl :: GitConfig -> String
buildUrl config = protocol config ++ "://" ++ host config
