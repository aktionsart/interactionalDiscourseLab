#' produces a fake meeting in the form of a data frame
#' @param nSpeakers number of speakers (default: 4)
#' @param nTags number of tags (default: 5)
#' @param nTurns  number of turns (default: 50)
#' @param seed random seed. Defaults to NULL
#'
#' @return a data frame with 3 fields:
#' speaker
#' speech (random letters and punctuation)
#' tag
#' @export
simulateMeeting <- function(seed = NULL, nSpeakers = 4, nTurns = 50, nTags = 5){

  if(!is.null(seed)) set.seed(seed)

  # from most popular names in the UK https://en.wikipedia.org/wiki/List_of_most_popular_given_names
  speakers = c("Oliver", "Jack", "Harry", "Jacob", "Charlie","Thomas","Oscar","William","James","George",
  "Amelia", "Olivia", "Emily", "Jessica", "Ava","Isla","Poppy","Isabella","Sophie","Mia") %>% sample(nSpeakers)

  tags =c("question", "greeting","repeat","check", "confirmation","agree", "disagree", "initiate", "state", "explanation",
          "qualify", "suggestion", "challenge", "joke", "offer", "refusal", "decision", "end", "closing", "proposal") %>% sample(nTags)

  data.frame(
    speaker = sample(speakers,nTurns, prob = runif(nSpeakers), replace = TRUE),
    speech = replicate(nTurns, paste(sample(c(letters, ' ', ',', '.', '!', '?'), sample(20:200,1), replace=TRUE),collapse='')),
    tag = sample(tags,nTurns, prob = runif(nTags), replace = TRUE)
  )

}

# get top level tags from a list of tags
getTopLevelTags <- function(s){
  stringr::str_extract(s,"^[^,]*")
}


# same as dplyr::count but includes counts of 0
count0 <- function(x, ..., wt = NULL, sort=FALSE){
  vars <- lazyeval::lazy_dots(...)
  combinations <- lapply(vars, function(l) levels(x[,as.character(l$expr)]))
  names(combinations) <- sapply(vars, function(l) as.character(l$expr))
  combinations <- expand.grid(combinations)

  wt <- substitute(wt)
  suppressMessages(dplyr::count_(x, vars, wt, sort = sort) %>% dplyr::right_join(combinations) %>% dplyr::mutate(n = (function(x) {x[is.na(x)]<-0;x})(n)))
}

# from list of top level tags to selected tags
getSelectedTagsFromTopLevelTags <- function(allTags, selectedTopLevels){
  x <- stringr::str_extract(allTags, "^[^,]*")
  index <- which(x %in% selectedTopLevels)
  unique(allTags[index])
}

# parses a text file and populates a data frame
# @param filename path and filename of the transcript
#
# @return a data frame with:
# speakers: name of the speaker
# tags: name of the tag, NA if none
parses <- function(filename){
  connection <- file(filename, 'r', blocking = TRUE)
  content <- readLines(connection, warn=FALSE)
  close(connection)

  speakers <- stringr::str_extract(content,"^[A-Z,a-z,0-9]+")
  tags <- stringr::str_replace_all(stringr::str_extract(content,"/[^/]*/"), " ", "");
  tags <- sapply(
    tags,
    function(x)
      if(is.na(x)) NA else stringr::str_sub(x,start=2,end=-2)
  )

  df <- data.frame(
    speaker = speakers,
    tag = tags)
  index <- which(is.na(df$speaker))

  if(length(index) == 0)
    return(df)
  # deals with multilines: reduce to one line and keep the latest tag, if any.
  for(k in rev(index)){
    if(!is.na(df$tag[k]))
      df$tag[k - 1] <- df$tag[k]
  }
  df <- df[-index,]
  df
}

# tidy up the parsed file
# @param parsed: the data frame produced by \code{parses(filename)} (speaker, tag)
#
# returns a data frame with
# tag: NA are replaced by "NA" (string)
# speaker: NA are replaced by "NA" (string)
# tags and speakers are factors
# turn: index of the turn
cleanupParsed <- function(parsed){
  parsed$tag <- sapply(as.character(parsed$tag) ,
                       function(s) if(is.na(s))  "NA" else s)

  parsed$speaker <- sapply(as.character(parsed$speaker) ,
                           function(s) if(is.na(s))  "NA" else s)

  parsed$tag <- factor(parsed$tag)
  parsed$speaker <- factor(parsed$speaker)
  parsed$turn <- 1:nrow(parsed)
  parsed

}

# builds a data.frame with each turn taking
# @param filteredData:  a data frame with fields "speaker", "tag" and "turn"
#
# @return Value
# returns a data frame with:
# speaker1, tag1, speaker2, tag2
#
# there is a turn taking iff the two turns were consecutive in the original data;
# two turns could end up being consecutive after removing some tags, which would be misleading.
makeTurnTaking <- function(filteredData){
  nLines <- nrow(filteredData)
  turnTaking <- data.frame(
    speaker1 = head(filteredData$speaker,nLines-1),
    tag1 = head(filteredData$tag,nLines-1),
    speaker2 = tail(filteredData$speaker,nLines-1),
    tag2 = tail(filteredData$tag,nLines-1) ,
    turnDiff = tail(filteredData$turn, nLines - 1 ) - head(filteredData$turn, nLines - 1 )
  )

  # limit to consecutive tags
  # turnTaking <- subset(turnTaking, turnDiff == 1)
  turnTaking %>% dplyr::filter(turnDiff == 1) %>% dplyr::select(-turnDiff)

}

makeConsecutiveTaggedTurns <- function(v){
  # v a boolean vector
  # returns vector of the same size, with each period where v is TRUE

  n <- length(v)
  period <- 1
  for(k in 1:(n-1))
    if(v[k]) {
      v[k] <- period
      if(!v[k+1]) period <- period + 1
    }
  if(v[n]) v[n] <- period
  v
}

# # # # # # # # # # # # #      interactions   # # # # # # # # # # # # #
countTagSequence <- function(turnTaking){
  turnTaking %>% dplyr::count(tag1, tag2) %>% dplyr::group_by(tag1) %>% dplyr::mutate(proportion = 100*n/sum(n)) %>% dplyr::arrange(tag1)
}

countTopLevelTagSequence <- function(turnTaking){
  turnTaking %>% dplyr::mutate(TL1 = getTopLevelTags(tag1), TL2 = getTopLevelTags(tag2)) %>%
    dplyr::count(TL1, TL2) %>%
    dplyr::group_by(TL1) %>%
    dplyr::mutate(proportion = 100*n/sum(n)) %>%
    dplyr::arrange(TL1)
}

countSpeakerSequence <-  function(turnTaking) {
  turnTaking %>% dplyr::count(speaker1, speaker2) %>% dplyr::group_by(speaker1) %>% dplyr::mutate(proportion = 100*n/sum(n)) %>% dplyr::arrange(speaker1)
}

addConfidenceIntervals <- function(sequence, itemName){
  lowerCI <- function(x) MultinomialCI::multinomialCI(x, 0.05)[,1]
  upperCI <- function(x) MultinomialCI::multinomialCI(x, 0.05)[,2]
  sequence %>% dplyr::group_by_(paste0(itemName,1)) %>% dplyr::mutate(lowerCI = 100*lowerCI(n), upperCI = 100*upperCI(n))
}

makeInteractionTable <- function(sequence, itemName){
  sequence %>% dplyr::mutate(content = sprintf("%3.2f [%3.2f %3.2f]",proportion, lowerCI, upperCI)) %>%
    dplyr::select_(paste0(itemName, 1), paste0(itemName, 2), "content") %>% tidyr::spread_(paste0(itemName, 2), "content", fill = "")
}
