flesch_reading_ease <- function(nwords, nsentence, nsyllables){
  score = 206.835 - 1.015 * (nwords/nsentences) - 84.6 * (nsyllables/nwords)
  return(score)
}

pct_unique_words <- function(text, nwords){
  unique = length(unique(unlist(str_split(text, ' '))))
  pct_unique = unique/nwords
  
  return(pct_unique)
}