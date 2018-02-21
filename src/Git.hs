{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Git where

import qualified GitConfig as GC
import qualified Data.ByteString.Lazy as BSL
import Network.Wreq
import GHC.Generics
import Control.Lens
import Data.Aeson (Value, FromJSON, ToJSON, encode)
import Data.Aeson.Lens (_String, key, _JSON, _Bool)
import Data.Text.Encoding
import Data.Text
import Text.RawString.QQ
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Maybe as DM

getReposJSON :: GC.GitConfig -> IO String
getReposJSON config = do
  repos <- getRepos' (GC.buildUrl config) (GC.token config) Nothing []
  return $ getJson $ Alfreds $ createAlfred repos

getJson :: ToJSON a => a -> String
getJson d = unpack $ decodeUtf8 $ BSL.toStrict (encode d)

getRepos' :: String -> String -> Maybe String -> [Repo] -> IO [Repo]
getRepos' url token cursor accu = do
  let opts = defaults & header "Authorization" .~ [BS8.pack $ " token " ++ token]
      withContentType = opts & header "Content-Type" .~ [BS8.pack "application/json"]
  response <- postWith withContentType (url ++ "/graphql") (BS8.pack $ getBody cursor)
  let repos = getReposJson response 
  return $ show $ getCursor response
  case getCursor response of Nothing -> return $ accu ++ repos
                             c       -> getRepos' url token c $ accu ++ repos
                           

getBody :: Maybe String -> String
getBody maybeCursor =
  [r|
     {"query": "query { viewer { name repositories(first:100 |] ++ 
     getCursorQuery maybeCursor ++ 
  [r|) { nodes { name url } totalCount pageInfo { endCursor hasNextPage }}}}"}
|]

getCursorQuery :: Maybe String -> String
getCursorQuery Nothing = ""
getCursorQuery (Just c) = [r|after:\"|] ++ c ++ [r|\"|]

getCursor :: Response BSL.ByteString -> Maybe String
getCursor response =
    if hasNext response then Just $ endCursor response else Nothing

getReposJson :: Response BSL.ByteString -> [Repo]
getReposJson response = response ^. responseBody . key "data" . key "viewer" . key "repositories" . key "nodes" . _JSON

hasNext :: Response BSL.ByteString -> Bool
hasNext response = DM.fromMaybe False $ response ^? responseBody . key "data" . key "viewer" . key "repositories" . key "pageInfo" . key "hasNextPage" . _Bool

endCursor :: Response BSL.ByteString -> String
endCursor response = T.unpack $ response ^. responseBody . key "data" . key "viewer" . key "repositories" . key "pageInfo" . key "endCursor" . _String


createAlfred :: [Repo] -> [Alfred]
createAlfred = fmap mapToAlfred

mapToAlfred :: Repo -> Alfred
mapToAlfred repos = Alfred {
  title = name repos,
  uid = name repos,
  arg = url repos,
  icon = Icon { path = "" }
}

data Repo = Repo {
  name :: String,
  url  :: String
} deriving (Generic, Show)
 
instance FromJSON Repo
instance ToJSON Repo

data Alfred = Alfred {
  title :: String,
  uid   :: String,
  arg   :: String,
  icon  :: Icon
} deriving (Generic, Show)                     

data Icon = Icon {
  path :: String
} deriving (Generic, Show)

data Alfreds = Alfreds {
  items :: [Alfred]
} deriving (Generic, Show)

instance ToJSON Alfred
instance ToJSON Icon
instance ToJSON Alfreds
