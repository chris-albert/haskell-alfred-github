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
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Maybe as DM

getReposJSON :: GC.GitConfig -> IO (String, Int)
getReposJSON config = do
  repos <- getRepos' (GC.org config) (GC.buildUrl config) (GC.token config) Nothing []
  return (getJson $ Alfreds $ createAlfred repos, Prelude.length repos)

getJson :: ToJSON a => a -> String
getJson d = unpack $ decodeUtf8 $ BSL.toStrict (encode d)

getRepos' :: String -> String -> String -> Maybe String -> [Repo] -> IO [Repo]
getRepos' org url token cursor accu = do
  let opts = defaults & header "Authorization" .~ [BS8.pack $ "Bearer " ++ token]
      withContentType = opts & header "Content-Type" .~ [BS8.pack "application/json"]
      graphqlBody = getBodySearch org cursor
--  putStrLn(graphqlBody)
  response <- postWith withContentType (url ++ "/api/graphql") (BS8.pack graphqlBody)
--  putStrLn (C.unpack (response ^. responseBody))
  let repos = getReposJsonSearch response 
  return $ show $ getCursor response
  case getCursor response of Nothing -> return $ accu ++ repos
                             c       -> getRepos' org url token c $ accu ++ repos

getBody :: Maybe String -> String
getBody maybeCursor =
  [r|
     {"query": "query { viewer { name repositories(first:100 affiliations: [OWNER,COLLABORATOR,ORGANIZATION_MEMBER] ownerAffiliations: [OWNER, COLLABORATOR, ORGANIZATION_MEMBER] |] ++ 
     getCursorQuery maybeCursor ++ 
  [r|) { nodes { name url } totalCount pageInfo { endCursor hasNextPage }}}}"}
|]

getBodySearch :: String -> Maybe String -> String
getBodySearch org maybeCursor =
  [r|
     {"query": "query { search(query: \"org:" ++ org ++ "\", type: REPOSITORY, first: 100 |] ++
     getCursorQuery maybeCursor ++ 
  [r|) { repositoryCount edges { node { ... on Repository { name url } } } pageInfo { endCursor hasNextPage }}}"}
|]

getCursorQuery :: Maybe String -> String
getCursorQuery Nothing = ""
getCursorQuery (Just c) = [r|after:\"|] ++ c ++ [r|\"|]

getCursor :: Response BSL.ByteString -> Maybe String
getCursor response =
    if hasNext response then Just $ endCursor response else Nothing

getReposJson :: Response BSL.ByteString -> [Repo]
getReposJson response = response ^. responseBody . key "data" . key "viewer" . key "repositories" . key "nodes" . _JSON

getReposJsonSearch :: Response BSL.ByteString -> [Repo]
getReposJsonSearch response = fmap (\rn -> node rn) (getRepoNodesJson response)

getRepoNodesJson :: Response BSL.ByteString -> [RepoNode]
getRepoNodesJson response = response ^. responseBody . key "data" . key "search" . key "edges" . _JSON

hasNext :: Response BSL.ByteString -> Bool
hasNext response = DM.fromMaybe False $ response ^? responseBody . key "data" . key "search" . key "pageInfo" . key "hasNextPage" . _Bool

endCursor :: Response BSL.ByteString -> String
endCursor response = T.unpack $ response ^. responseBody . key "data" . key "search" . key "pageInfo" . key "endCursor" . _String


createAlfred :: [Repo] -> [Alfred]
createAlfred = fmap mapToAlfred

mapToAlfred :: Repo -> Alfred
mapToAlfred repos = Alfred {
  title = name repos,
  uid = name repos,
  arg = url repos,
  icon = Icon { path = "" }
}

data RepoNode = RepoNode {
  node :: Repo
} deriving (Generic, Show)

instance FromJSON RepoNode
instance ToJSON RepoNode

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
