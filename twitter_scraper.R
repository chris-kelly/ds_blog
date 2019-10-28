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

# https://developer.twitter.com/en/docs/tweets/search/guides/standard-operators

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
# 100 users per request
result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users'
                           , param_list = list(usernames = 'ChelseaFC,ManUtd'
                                               , format = 'detailed'))
# LEAGUE TABLE OF RESULTS
cbind(result$data$name, result$data$stats)

# get_twitter_channel_tweets (max num_tweets is 200)
# Returns a collection of the most recent Tweets posted by the user
# https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline
result <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                           , param_list = list(screen_name = 'ChelseaFC'
                                               , count = 200
                                               , tweet_mode = 'extended')) # tweet_mode=extended to prevent tweet truncation

# Returns a collection of relevant Tweets matching a specified query (Standard search api)
result <- generic_api_call(api = 'https://api.twitter.com/1.1/search/tweets.json'
                           , param_list = list(q = '#CFC'
                                               , count = 200
                                               , until = Sys.Date()-7
                                               , tweet_mode = 'extended'))


# Returns a cursored collection of user IDs for every user following the specified user.
result <- generic_api_call(api = 'https://api.twitter.com/1.1/followers/ids.json'
                           , param_list = list(screen_name = 'ChelseaFC'
                                               , cursor = -1
                                               , count = 5000))

# Returns a cursored collection of user IDs for every user the specified user is following (otherwise known as their "friends").
result <- generic_api_call(api = 'https://api.twitter.com/1.1/friends/ids.json'
                           , param_list = list(user_id = '1188535771910885376' # tweet id
                                               , cursor = -1
                                               , count = 5000))

# Returns a collection of up to 100 user IDs belonging to users who have retweeted the Tweet specified by the id parameter.
result <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweeters/ids.json'
                           , param_list = list(id = '327473909412814850' # tweet id
                                               , cursor = -1
                                               , count = 100))


########################################
## EXAMPLE CHELSEAFC TWEET TRACKING
########################################

recent_tweets <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                                  , param_list = list(screen_name = 'ChelseaFC'
                                                      , count = 200
                                                      , tweet_mode = 'extended'
                                                      , exclude_replies = 'true'))

convert_dates <- function(date) {
  return(as.POSIXct(date, format = '%a %b %e %H:%M:%S %z %Y', ))
}

recent_tweets$created_at_posixct <- sapply(recent_tweets$created_at, FUN = function(x) convert_dates(x))
class(recent_tweets$created_at_posixct) <- c('POSIXt','POSIXct')

require(plotly)

plot_ly(type = 'scatter', mode = 'bar') %>%
  add_trace(x = recent_tweets$created_at_posixct
            , y = recent_tweets$favorite_count
            , text = recent_tweets$full_text
            , name = 'favorite_count') %>%
  add_trace(x = recent_tweets$created_at_posixct
            , y = recent_tweets$retweet_count
            , text = recent_tweets$full_text
            , name = 'retweet_count'
            , yaxis = "y2") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")
         , legend = list(orientation = 'h'))


########################################
## EXAMPLE CHELSEAFC FOLLOWER LOCATIONS
########################################

# Returns a cursored collection of user IDs for every user following the specified user.
# Can run 15 per 15 mins

all_followers <- list()
cursor <- -1
i <- 1

while(length(call$ids) > 0) {
  call <- generic_api_call(api = 'https://api.twitter.com/1.1/followers/ids.json'
                           , param_list = list(screen_name = 'ChelseaFC'
                                               , cursor = -1
                                               , count = 5000))
  all_followers[[paste0('cursor_', cursor)]] <- call$ids
  cursor <- call$next_cursor
  print(i)
  i <- i+1
}


tryCatch(expr = a$errors2, error = function(e) {'hello'})

a <- call

exists(x = 'a$errors')

# https://developer.twitter.com/en/docs/tweets/search/api-reference/premium-search#Methods
# https://developer.twitter.com/en/docs/tweets/search/overview/premium#AvailableOperators
result <- generic_api_call(api = 'https://api.twitter.com/1.1/tweets/search/fullarchive/development.json'
                           , param_list = list(query = 'from:ChelseaFC'
                                               , maxResults = 100))
# DATA PAGINATION: result[['next']]


