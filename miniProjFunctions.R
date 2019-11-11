flesch_reading_ease <- function(nwords, nsentences, nsyllables){
  score = round((206.835 - 1.015 * (nwords/nsentences) - 84.6 * (nsyllables/nwords)), 2)
  return(score)
}

pct_unique_words <- function(text, nwords){
  unique = length(unique(unlist(str_split(text, ' '))))
  pct_unique = round((unique/nwords), 2)
  
  return(pct_unique)
}

exclamation_ratio <- function(text){
  end_point_count = str_count(text, "\\!") + str_count(text, "\\.") + str_count(text, "\\?")
  ratio = round((str_count(text, "\\!") / end_point_count)*100, 2)
  ratio = if_else(ratio == "NaN", 0.00, ratio)
  return(ratio)
}

max_sentiment_value <- function(text){
  sentences_vector = get_sentences(text)
  sentiment = get_nrc_sentiment(sentences_vector)
  anger = sum(sentiment$anger)
  anticipation = sum(sentiment$anticipation)
  disgust = sum(sentiment$disgust)
  fear = sum(sentiment$fear)
  joy = sum(sentiment$joy)
  sadness = sum(sentiment$sadness)
  surprise = sum(sentiment$surprise)
  trust = sum(sentiment$trust)
  negative = sum(sentiment$negative)
  positive = sum(sentiment$positive)
  
  sentiment_vec = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive)
  
  return(max(sentiment_vec))
}

max_sentiment_type <- function(text){
  sentences_vector = get_sentences(text)
  sentiment = get_nrc_sentiment(sentences_vector)
  anger = sum(sentiment$anger)
  anticipation = sum(sentiment$anticipation)
  disgust = sum(sentiment$disgust)
  fear = sum(sentiment$fear)
  joy = sum(sentiment$joy)
  sadness = sum(sentiment$sadness)
  surprise = sum(sentiment$surprise)
  trust = sum(sentiment$trust)
  negative = sum(sentiment$negative)
  positive = sum(sentiment$positive)
  
  sentiment_vec = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive)
  index = which.max(sentiment_vec)
  if(index == 1){
    return("anger")
  }
  else if(index == 2){
    return("anticipation")
  }
  else if(index == 3){
    return("disgust")
  }
  else if(index == 4){
    return("fear")
  }
  else if(index == 5){
    return("joy")
  }
  else if (index == 6){
    return("sadness")
  }
  else if (index == 7){
    return("surprise")
  }
  else if (index == 8){
    return("trust")
  }
  else if(index == 9){
    return("negative")
  }
  else if(index == 10){
    "positive"
  }
  return("??????")
  
  
}