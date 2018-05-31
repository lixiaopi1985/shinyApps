retrieve.twitter.user <- function(consumer_key, consumer_secret, access_token, access_secret, scn, ntweets, inclRt = TRUE){
  
  # load credential

  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
  
  user = getUser(scn)
  tweets = userTimeline(user, n = ntweets, includeRts = inclRt)
  
  df = twListToDF(tweets)
  
  # clean tweets
  # remove http links, stop words in the tweets
  #data("stop_words")
  #replacement = paste(stop_words$word, collapse = "|")
  # states
  states_abb = datasets::state.abb
  states_full = datasets::state.name
  statesPattern = paste0(states_full, "|", states_abb)
  
  
  pattern1 = "http://.+|https://.+"
  
  tweets_clean = df %>%
    mutate(clean_text = str_trim(str_replace(str_replace_all(tolower(str_trim(str_replace(text, pattern1, " "), "right")), "[[:punct:]]|[[0-9]]", " "), "^rt", ""), "both"))
  
  
  states = vector(length = nrow(tweets_clean))
  tweets_text = tweets_clean$text
  for(i in 1:length(tweets_text)){
    for(j in 1:length(statesPattern)){
      if(str_detect(tweets_text[i], statesPattern[j])){
        states[i] = unlist(str_split(statesPattern[j], "\\|"))[1]
      } 
    }
  }
  
  
  return(cbind(tweets_clean, states))
  
}