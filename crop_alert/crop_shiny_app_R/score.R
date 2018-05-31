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