library(twitteR)
library(stringr)
library(dplyr)
library(tidytext)
library(lubridate)
library(quanteda)
library(widyr)
library(ggraph)
library(purrr)
library(tm)
library(usmap)
library(stringdist)
library(tidyr)
library(ggmap)
library(ggrepel)
library("ROAuth")




retrieve.twitter.user <- function(consumer_key, consumer_secret, access_token, access_secret, userName, ntweets, inclRt = TRUE){
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  user = getUser(userName)
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


score = function(string, pattern){
  
  matching = unlist(str_split(tolower(pattern), "\\s|\\|"))

  score = 0
  
  if( str_detect(string, pattern) ){
    score = score + 20
    return(score)
  } else {
  
    for(i in 1:length(matching)){
    
    # perfect case
      if( i == 1 & grepl(matching[i], string)){
        score = score + 10
      } else if( i == 2 & grepl(matching[i], string)){
        score = score + 5
      } else if ( i == 3 & grepl(matching[i], string)){
        score = score + 4
      } else if ( i == 4 & grepl(matching[i], string)){
        score = score + 3
      } else if ( i == 5 & grepl(matching[i], string)){
        score = score + 2
      } else if ( i == 6 & grepl(matching[i], string)){
        score = score + 1
      } else {
        score = score + 0
      }
    }
    
  return(score)
    
  }
}



retrieve.twitter.term <- function(twitterDF, diseaseDB){
  
  # load disease list and reformat
  pathogen_df = read.delim(diseaseDB, header = FALSE, col.names = "common_names")
  pathogen = pathogen_df %>% 
    mutate(patho_names = str_replace(common_names, " or ", "|")) %>% 
    mutate(patho_names2 = str_replace(patho_names, " \\(and ", "|")) %>%
    mutate(patho_names3 = str_replace(patho_names2, " \\(", "|")) %>%
    mutate(patho_names4 = str_replace(patho_names3, "\\)", "")) %>% 
    mutate(patho_names5 = str_replace(patho_names4, " and ", " ")) %>%
    mutate(patho_names6 = str_replace(patho_names5, "'s", "")) %>% .[-c(109:142),] %>%
    select(patho_names6)
  
  
  # tweets text
  tw = twitterDF$clean_text
  path_names = pathogen$patho_names6
  
  # tweet date and states
  dates = as.Date(twitterDF$created)
  states = twitterDF$states
  
  # create a matrix
  mx = matrix(nrow = length(dates), ncol = length(pathogen$patho_names6), byrow = TRUE)
  
  
  for( i in 1:length(tw)){
    for( j in 1:length(path_names)){
      if(score(tw[i], path_names[j]) >= 19){
        mx[i, j] = path_names[j]
      } 
      }
    }
  
  
  dimnames(mx) <- list(as.character(dates), path_names)
  
  mx.df = data.frame(mx)
  mx.df = cbind(mx.df, dates = as.character(dates), states = states)
  mx.df = mx.df %>%
    gather(pathogen_list, pathogen_reported, 1:146, na.rm = TRUE) %>%
    select(dates, states, pathogen_reported) %>% arrange(desc(dates))
  
  # adding coordinates to the df
  states_names <- ggplot2::map_data("state")
  # get avg of the coordinate for each state, since we want to draw a dot on them
  # computer centroid with mean(range(long))
  states_av <- states_names %>% group_by(region) %>% summarise(avg_long = mean(range(long)), avg_lat = mean(range(lat)))
  # change the format
  states_av$region = str_to_title(states_av$region)
  colnames(states_av)[1] = "states"
  
  mx.df.cor = merge.data.frame(mx.df, states_av, by = "states")
  mx.df.cor = mx.df.cor %>% arrange(desc(dates))
  
  return(mx.df.cor)
  
}

retrieve.twitter.DA = function(mx, yr = "2017", mn = "9"){
  # aggregate
  mx.aggre = mx %>% group_by(dates, states, avg_long, avg_lat, pathogen_reported) %>% mutate(year = year(dates), month = month(dates)) %>% group_by(year, month, avg_long, avg_lat, pathogen_reported) %>% summarise(total = sum(n())) %>% arrange(desc(year), desc(month))
  
  
  mx.aggre.custom <- mx.aggre %>% filter(year == yr & month == mn)
  
  # load states
  states = map_data("state")
  basemap = ggplot() + geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey", col = 'white') + coord_fixed(1.3) + guides(fill = FALSE)
  
  outmap = basemap + 
    geom_point(data = mx.aggre.custom, aes(x = avg_long, y = avg_lat, size = total)) +
    geom_text_repel(data = mx.aggre.custom, aes(x = avg_long, y = avg_lat, label = pathogen_reported)) +
    ggtitle(paste0("Plant pathogen reported on twitter ", "(Year: ", yr, " Month:", mn, ")")) + theme(plot.title = element_text(hjust = 0.5))
  
  print(outmap)
}








