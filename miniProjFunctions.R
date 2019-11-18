#' flesch_reading_ease
#'
#' @param nwords number of total words in the text
#' @param nsentences number of total sentences in the text
#' @param nsyllables number of total syllables in the text
#'
#' @return Flesch Reading Ease score
flesch_reading_ease <- function(nwords, nsentences, nsyllables){
  score = round((206.835 - 1.015 * (nwords/nsentences) - 84.6 * (nsyllables/nwords)), 2)
  return(score)
}

#' pc_unique_words
#'
#' @param text text to analyze
#' @param nwords number of total words
#'
#' @return number of unique words
pct_unique_words <- function(text, nwords){
  unique = lengths(lapply(strsplit(text, split = ' '), unique))
  pct_unique = round(unique/nwords, 2)
  
  return(pct_unique*100)
}

#' exclamation_ratio
#'
#' @param text text to analyze
#'
#' @return ration of exclamation points over all sentence ending punctuation 
exclamation_ratio <- function(text){
  end_point_count = str_count(text, "\\!") + str_count(text, "\\.") + str_count(text, "\\?")
  ratio = round((str_count(text, "\\!") / end_point_count)*100, 2)
  ratio = if_else(ratio == "NaN", 0.00, ratio)
  return(ratio)
}

#' max_sentiment_value
#'
#' @param text text to analyze sentiment of
#' @param n which value to return in higherarchy of sentiment
#'
#' @return pct of sentences with nth strongest sentiment
max_sentiment_value <- function(text, n){
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
  n_largest = sort(sentiment_vec, decreasing=T)[n]
  
  return(round(n_largest/sum(sentiment), 2)*100)
}

#' max_sentiment_type
#'
#' @param text  text to analyze sentiment of
#' @param n  which value to return in higherarchy of sentiment
#'
#' @return nth strongest sentiment in the text
max_sentiment_type <- function(text, n){
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
  
  sentiment_vec = order(c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive), decreasing = T)
  return(switch(sentiment_vec[n], "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"))
  
}

'%!in%' <- function(x, y){!('%in%'(x,y))}

