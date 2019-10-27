require(jsonlite)
require(logging)

twitter_oauth2 <- function(consumer_api_key, consumer_secret_api_key) {

  ## Check if hidden twitter environment exists
  if(!(exists('.env_twitter'))) {
    loginfo('Making first authentication in this session: building hidden environment')
    .env_twitter <<- new.env(parent=baseenv())
  }

  ## Retrieve access token
  .env_twitter$access_token <- fromJSON(system(paste0("curl -u '"
                                                      , consumer_api_key, ":", consumer_secret_api_key
                                                      , "' --data 'grant_type=client_credentials' 'https://api.twitter.com/oauth2/token'")
                                               , intern = T))
  
  ## Create header for requests
  .env_twitter$request_header <- paste0('curl -X GET -H "Authorization: Bearer ', .env_twitter$access_token$access_token, '" "')
  
}

twitter_oauth2('XRWBeVs7sWQ0vpCpfSX9L1VPw', 'Jso9nW10QTiZolbiJtUpb66fimoEZEZXYAuAiTihUzJBKdf9sB')

generic_api_call <- function(api = 'https://api.twitter.com/labs/1/users'
                             , param_list = list(usernames = 'ChelseaFC'
                                                 , format = 'detailed')) {
  params <- lapply(param_list, FUN = function(param) gsub(pattern = '#', replacement = '%23', URLencode(as.character(param))))
  result <- fromJSON(system(paste0(.env_twitter$request_header
                                   , api, '?'
                                   , paste0(paste0(names(params), '=', params), collapse = '&')
                                   , '"')
                            , intern = T))
  return(result)
  
}

# get_channel_stats
# Returns a variety of information about one or more Users specified
# https://developer.twitter.com/en/docs/labs/tweets-and-users/api-reference/get-users-v1
result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users'
                           , param_list = list(usernames = 'ChelseaFC'
                                               , format = 'detailed'))

# get_twitter_channel_tweets (max num_tweets is 200)
# Returns a collection of the most recent Tweets posted by the user
# https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline
result <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                           , param_list = list(screen_name = 'ChelseaFC'
                                               , count = 200
                                               , tweet_mode = 'extended')) # tweet_mode=extended to prevent tweet truncation

# Returns a collection of relevant Tweets matching a specified query (Standard search api)
result <- generic_api_call(api = 'https://api.twitter.com/1.1/search/tweets.json'
                           , param_list = list(q = 'Chelsea Football'
                                               , count = 200
                                               , until = Sys.Date()-7
                                               , tweet_mode = 'extended'))
