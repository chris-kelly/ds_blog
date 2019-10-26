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

get_channel_stats <- function(channel = 'ChelseaFC') {
  # max num_tweets is 200
  # https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline
  result <- fromJSON(system(paste0(.env_twitter$request_header
                                   , 'https://api.twitter.com/labs/1/users?'
                                   , 'usernames=', channel
                                   , '&format=detailed'
                                   , '"')
                            , intern = T))
  return(result)
}

get_twitter_channel_tweets <- function(channel = 'ChelseaFC', num_tweets = 200) {
  # max num_tweets is 200
  # https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline
  result <- fromJSON(system(paste0(.env_twitter$request_header
                                   , 'https://api.twitter.com/1.1/statuses/user_timeline.json?'
                                   , 'screen_name=', channel
                                   , '&count=', num_tweets
                                   , '"')
                            , intern = T))
  return(result)
}

search_twitter_query <- function(query = 'Chelsea Football', num_tweets = 100, until = Sys.Date()-7) {
  # max num_tweets is 100
  # https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline
  result <- fromJSON(system(paste0(.env_twitter$request_header
                                   , 'https://api.twitter.com/1.1/search/tweets.json?'
                                   , 'q=', URLencode(query)
                                   , '&count=', num_tweets
                                   # , '&until=', until
                                   , '"')
                            , intern = T))
  return(result)
}