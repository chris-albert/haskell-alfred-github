# haskell-alfred-github

This is used in an alfred script to be able to search your repos easily. The idea here is 
that getting all your repos from github is slow, so first you store all your github repos 
locally, then when you are typing in alfred it gets the locally cached version, which is fast
enough for typeahead.

In alfred i have it set up so `gh your-repo-name` will type ahead your repos,
and `gh-update` will go to github and cache your data to whatever you have configured
in your config file.

Also since this was done real fast, i'm not handling when your cache file doesn't exist,
so you must create it first. Then ever time you do a `store` it overwrites it.

You must have a config file in the format
```
protocol = https
host = api.github.com
token = your-github-token-here
cachefile = file-to-put-your-github-repos-cache (~ is not supported here, must be absolute path)
org = github organization
```

Save repos to disk
```
./haskell-alfred-github -m store ~/.path-to-config-file
```

Get caches repos from disk
```
./haskell-alfred-github -m get ~/.path-to-config-file
```


The GraphQL query (this doesn't seem to work...)
```
query {
  viewer {
    name repositories(first:100 affiliations: [OWNER, COLLABORATOR, ORGANIZATION_MEMBER] ownerAffiliations: [OWNER, COLLABORATOR, ORGANIZATION_MEMBER]) {
      nodes {
        name url
      }
    }
  }
}
```
but this does 
```
{
  search(query: "org:baseball-data", type: REPOSITORY, first: 100) {
    repositoryCount
    edges {
      node {
        ... on Repository {
          name url
        }
      }
    }
    pageInfo {
      endCursor
      startCursor
    }
  }
}    

```


```
stack build --file-watch
stack exec haskell-alfred-github -- store -c ~/.alfred-github.conf
```
