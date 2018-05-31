retrieve.twitter.term <- function(twitterDF, diseaseDB, yr, mn){
  
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
  
  mx.aggre = mx.df.cor %>% group_by(dates, states, avg_long, avg_lat, pathogen_reported) %>% mutate(year = year(dates), month = month(dates)) %>% group_by(year, month, avg_long, avg_lat, pathogen_reported) %>% summarise(total = sum(n())) %>% arrange(desc(year), desc(month))
  
  
  mx.aggre.custom <- mx.aggre %>% filter(year == yr & month == mn)
  
  return(mx.aggre.custom)
  
}