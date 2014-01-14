{-# LANGUAGE OverloadedStrings #-}
-- | The organization teams API as described on
-- <http://developer.github.com/v3/orgs/teams/>.
module Github.Organizations.Teams (
 createTeam
,addTeamMember
,addTeamRepo
,GithubAuth(..)
,TeamPermission(..)
,NewTeam(..)
,Team(..)
,module Github.Data
) where

import Control.Applicative
import Data.Aeson.Types
import Github.Data
import Github.Private

data TeamPermission = TeamPull | TeamPush | TeamAdminister deriving Show

instance FromJSON TeamPermission where
  parseJSON (String "pull")  = return TeamPull
  parseJSON (String "push")  = return TeamPush
  parseJSON (String "admin") = return TeamAdminister
  parseJSON _ = fail "Could not build a TeamPermission"

data Team = Team {
   teamMembersCount :: Integer
 , teamReposCount :: Integer
 , teamSlug :: String
 , teamID :: Integer
 , teamPermission :: TeamPermission
 } deriving Show

instance FromJSON Team where
  parseJSON (Object o) =
    Team <$>
    o .: "members_count" <*>
    o .: "repos_count" <*>
    o .: "slug" <*>
    o .: "id" <*>
    o .: "permission"

data NewTeam = NewTeam {
   newTeamName :: String
 , newTeamRepoNames :: Maybe [String]
 , newTeamPermission :: Maybe TeamPermission
 } deriving Show

instance ToJSON TeamPermission where
  toJSON TeamPull = "pull"
  toJSON TeamPush = "push"
  toJSON TeamAdminister = "admin"

instance ToJSON NewTeam where
  toJSON (NewTeam { newTeamName        = name
                  , newTeamRepoNames   = repo_names
                  , newTeamPermission  = permission }) =
         object
           [ "name" .= name
           , "repo_names" .= repo_names
           , "permission" .= permission]
-- | Create a team
--
-- > createTeam (GithubBasicAuth user pass) orgname
-- >   (NewTeam {
-- >      newTeamName = "fooTeam"
-- >    , newTeamRepoNames = Just ["foorepo","barrepo"]
-- >    , newTeamPermission = TeamPush}) 
createTeam :: GithubAuth -> String -> NewTeam -> IO (Either Error Team) 
createTeam auth org newteam = githubPost auth ["orgs", org, "teams"] newteam
  

-- | Add a member to a team
--
-- > addTeam (GithubBasicAuth user pass) id addeduser
addTeamMember :: GithubAuth -> Integer -> String -> IO ()
addTeamMember auth teamid member = githubPut auth ["teams", show teamid, "members", member]

-- | Add a repo to a team
--
-- > addTeamRepo (GithubBasicAuth user pass) id repoorg repo
addTeamRepo :: GithubAuth -> Integer -> String -> String -> IO ()
addTeamRepo auth teamid org repo = githubPut auth ["teams", show teamid, "repos", org, repo]

