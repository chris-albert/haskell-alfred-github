module Main where

import qualified GitConfig as GC
import qualified Git as G
import qualified Args
import qualified Dispatch

import Control.Monad.Except
import System.Environment
import Control.Exception

main :: IO ()
main = do
  out  <- catchAny run
  putStrLn out

run :: IO String
run = do
  args   <- Args.getArgs
  config <- GC.readConfig $ getConfFile args
  Dispatch.dispatch args config

getConfFile :: Args.ArgsMode -> String
getConfFile (Args.Store cf) = cf
getConfFile (Args.Get cf) = cf

headOption :: [a] -> Maybe a
headOption [] = Nothing
headOption (h:_) = Just h

liftMaybe :: Maybe a -> IO a
liftMaybe (Just a) = return a
liftMaybe Nothing = error "Maybe was empty"

catchAny :: IO String -> IO String
catchAny io = (Control.Exception.catch io)
              (\e -> return $ show (e :: SomeException))
