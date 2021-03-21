# Author:         Trevor Strobel
# File:           set-Relation.R

library(plumber)
library(jsonlite)
library(set)



#TODO: 3/15/21 clean up redundant code. need a makeSets(n,m) function instead of 
#       repeating the set gen code for each operation.

options_plumber(
  port = 3157
)
#The purpose of this file is provide an API from which
# a web browser may request a problem to solve regarding
# set operations. 


#The following is pretty unsafe and is only planned to be used during early development. 
#It allows for Cross Origin Resource Sharing from any client. 
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

# getSetUnion(n) generates and prepares n sets
# of size m as well as 3 false "answers" and 1 
# correct "answer" when considering the union of
# said sets. 
#* @param  n     The number of sets to consider
#* @param  m     The number of elements in each set. 
# @return json  A json object containing the
#               sets, correct, and incorrect
#               answers.
#* @get  /getSetUnion
getSetUnion <- function(n=2, m=5) {
  wrongs <- list() #creates an empty list of wrong answers
  iWrongs <- 1     #index of wrong answer list.
  
  if(is.null(m) && is.null(n)){
    numEntries <- n*m 
  } else {
    numEntries = 10
  }
  #creates a single vector. Ints ranging 1 to 20. 20 ints in the vector.

  sets <- list()    #creating an empty list. Elements will be sets 

  #for each in a given number of sets
  for(e in (1:n)) {
    sets[[e]] <- sample(1:20, m, replace = T)
  }

  #only going to work for 2 sets so far. 
  answer <- union(sets[[1]], sets[[2]])
  answer <- answer[order(answer)]

  
  #The following decides if there was a duplicate in the original sets.
  # If so, it provides the union with duplicates to be used as an incorrect
  # answer. 
  dupeSets <- numeric(0) #creates an empty vector
  
  for (e in (1:n)) {
    dupeSets <- c(dupeSets, sets[[e]])
  }
  
  #if 'answer' and 'dupeSets' are of different length, then dupeSets
  # can be provided as an incorrect answer.
  if(length(dupeSets) != length(answer)) { 
    #add dupeSets to the list of incorrect answers
    wrongs[[iWrongs]] <- sort(dupeSets, decreasing = FALSE)
    iWrongs <- iWrongs + 1
    
  }
  
  #sorts and adds wrong answers to wrongs list.
  for(e in (iWrongs:3)) {
    wrongs[[iWrongs]] <- sort((sample(1:20, (numEntries), replace = T)), decreasing = FALSE)
    iWrongs <- iWrongs + 1
  }
  
  #TODO: remove. for debugging  
  for (e in wrongs) {
    print(sort(e, decreasing = FALSE))
  }
  
  sets <- c("Select the correct union of the following sets", sets)
  
  #format answers and sources into json and return results 
  toSend <- list(source= sets, answer= answer, wrongs= wrongs)
  
  jsonToSend <- toJSON(toSend)
  
  return(jsonToSend)
  
}




#getSetIntersect(n, m) generates and prepares n sets
# of m integers, 3 "distractors" and 1 correct answer
# when considering the intersections of said sets.
#TODO: CURRENTLY ONLY SUPPORTS TWO SETS.

#* @param  n     The number of sets to consider
#* @param  m     The number of elements in each set. 
# @return json  A json object containing the
#               sets, correct, and incorrect
#               answers.
#* @get /getSetIntersect
getSetIntersect <- function(n=2, m=5) {
  
  n <-2   #currently, the api only supports 2 sets. 
  
  sourceSets <- list()    #creating an empty list. Elements will be sets 
  
  #for each in a given number of sets, fill the set with ints (1:10)
  for(e in (1:n)) {
    sourceSets[[e]] <- sample(1:9, m, replace = F)
  }
  
  allElements <- vector()
  
 
  for(e in sourceSets) {
 
    allElements <- c(allElements, e)
  }
  
  #only going to work for 2 sets so far. 
  answer <- intersect(sourceSets[[1]], sourceSets[[2]])
  answer <- answer[order(answer, decreasing = FALSE)]
  
  
  #Distractors
 
  # distractor 1 (d1) includes 2 copies of all elements in the answer. 
  # This tests the users knowledge of whether intersection should include 
  # duplicates in the answer. 
  d1 <- c(answer, answer)
  #print("D1")
  #print(d1)
  d1 <- sort(d1, decreasing = FALSE)
  
  
  
  # Distractor 2 (d2) includes all the elements from both sets. 
  # A student not understanding the difference between union and 
  # intersection would miss this. 
  d2 <- allElements
  
  
  # Distractor 3 (d3) includes the set difference of the source sets.
  #d3 <- dif
  
  
  d3 <- not(sourceSets[[1]], sourceSets[[2]])
  d3 <- c(d3, not(sourceSets[[2]], sourceSets[[1]]))
  
  #adding all disctractors to "wrongs" vector.
  
  wrongs <- list()
  wrongs[[1]] <- d1
  wrongs[[2]] <- d2
  wrongs[[3]] <- d3
 
  questionStr <- "Select the correct intersection of the following sets"
  
  sourceSets <- c(questionStr, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(source= sourceSets, answer= answer, wrongs= wrongs)
  
  jsonToSend <- toJSON(toSend)
  
  return(jsonToSend)
  
}





# getSetDiff(n, m) genereates and prepares n sets
# of m integers, 3 "distractors" and 1 correct answer
# when considering the difference of the generated
# sets.
# 
# NOTE: the results reflect A-B where A is the first set in
# 'source' and B is the second set in 'source'

#* @param  n     The number of sets to consider
#* @param  m     The number of elements in each set. 
#* @response json  A json object containing the
#               sets, correct, and incorrect
#               answers.
#* @get /getSetDiff
getSetDiff <- function(n=2, m=5) {
  
  n <-2   #currently, the api only supports 2 sets. 
  
  sourceSets <- list()    #creating an empty list. Elements will be sets 
  
  #for each in a given number of sets, fill the set with ints (1:9)
  for(e in (1:n)) {
    sourceSets[[e]] <- sample(1:9, m, replace = F)
  }
  allElements <- vector()
  
  for(e in sourceSets) {
    print(e)
    allElements <- c(allElements, e)
  }
  
  answer <- not(sourceSets[[1]], sourceSets[[2]])
  
  
  # Distractor 1 (d1) is the difference of A-B and the difference B-A
  d1 <- c(answer, not(sourceSets[[2]], sourceSets[[1]]))
  d1 <- sort(d1, decreasing = FALSE)
  
  # Distractor 2 (d2) is the difference B-A (the correct is A-B)
  d2 <- not(sourceSets[[2]], sourceSets[[1]])
  d2 <- sort(d2, decreasing = FALSE)
  
  # Distractor 3 (d3) is the intersection of sets A and B
  d3 <- intersect(sourceSets[[1]], sourceSets[[2]])
  
  
  wrongs <- list()
  wrongs[[1]] <- d1
  wrongs[[2]] <- d2
  wrongs[[3]] <- d3
  
  
  # in the case that the source sets match, distractor answers would be empty as well as the correct answer.
  # this just fills those empty answers with randomly generated sets. 
  
  #TODO: 3/21/21 TJS. It's not very elegant. Maybe we offer
  # only 2 answer choices in this case. Front end would need to handle display issues though. 
  for(e in wrongs) {
    if(e == "[]"){
      e <- sample(1:9, m, replace = F)
    }
  }
  
  
  questionStr <- "Select the correct set difference A-B where the first set is A and the second is B"
  
  sourceSets <- c(questionStr, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(source= sourceSets, answer= answer, wrongs= wrongs)
  
  jsonToSend <- toJSON(toSend)
  
  return(jsonToSend)
  
}