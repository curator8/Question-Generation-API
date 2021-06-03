# Author:         Trevor Strobel
# File:           set-Relation.R

library(plumber)
library(jsonlite)
library(set)
library(nsprcomp)


source("utils/format.R") #set string formatting
source("utils/set-generation.R") #set generation


# This file describes individual problems related to Set Theory




# getSetUnionMC() generates and prepares a number
# of sets as well as 3 "distractors" and 1 
# correct "answer" when considering the union of
# said sets. 
#
# param   n       The number of sets to consider
# param   m       The number of elements in each set. 
# param   dType   The desired data type for set elements
#                 (1: Ints, 2: Real, 3: Complex, 
#                  4: Char, 5: String, 6: Mixed)               
#
# return  toSend  A json-like object containing the
#                 sets, correct, and incorrect
#                 answers.

getSetUnionMC <- function(n=2, m=5, dType = 1) {
  wrongs <- list() #creates an empty list of wrong answers
  iWrongs <- 1     #index of wrong answer list.
  
  if(is.null(m) && is.null(n)){
    numEntries <- n*m 
  } else {
    numEntries = 10
  }
  
  # fill sets with data
  sets <- getSets(x = dType)
  
  #only going to work for 2 sets so far. 
  answer <- union(sets[[1]], sets[[2]])
  
  

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
    wrongs[[iWrongs]] <- formatListAsSet(dupeSets)
    iWrongs <- iWrongs + 1
    
  }
  
  #adds wrong answers to wrongs list.
  for(e in (iWrongs:3)) {
    current <- (sample(1:20, (numEntries), replace = F))
    wrongs[[iWrongs]] <- formatListAsSet(current)
    iWrongs <- iWrongs + 1
  }
  
  
 #TODO: comment this
  counter <- 1
  for(s in sets){
    current <- formatListAsSet(s)
    sets[counter] <- current
    counter <- counter + 1
  }
  
  #adds mathjax formatting to string
  answer <- formatListAsSet(answer)
  
  #avilk: sets modified in order to have them displayed with the text.
  sets <- insertSetQStrings(sets) 

  qstn<-('Let A and B be two sets. What is \\$A\\cup B\\$?')
  
  
  #avilk: the question was moved to the end of the list 
  sets <- c( qstn, sets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= sets, correct= answer, distractors= wrongs)
  
  
  return(toSend)
  
}




# getSetIntersect() generates and prepares n sets
# of m elements, 3 "distractors" and 1 correct answer
# when considering the intersections of said sets.
#
# param   n       The number of sets to consider
# param   m       The number of elements in each set. 
# param   dType   The desired data type for set elements
#                 (1: Ints, 2: Real, 3: Complex, 
#                  4: Char, 5: String, 6: Mixed)               
#
# return  toSend  A json-like object containing the
#                 sets, correct, and incorrect
#                 answers.

getSetIntersectMC <- function(n=2, m=5, dType = 1) {
  
  n <-2   #currently, the api only supports 2 sets. 
  sourceSets <- getSets(x = dType)  #fills lists with data
  
  allElements <- vector()
  
 
  for(e in sourceSets) {
 
    allElements <- c(allElements, e)
  }
  
  #only going to work for 2 sets so far. 
  answer <- intersect(sourceSets[[1]], sourceSets[[2]])

  
  
  #Distractors
 
  # distractor 1 (d1) includes 2 copies of all elements in the answer. 
  # This tests the users knowledge of whether intersection should include 
  # duplicates in the answer. 
  d1 <- c(answer, answer)
  
  
  
  
  
  # Distractor 2 (d2) includes all the elements from both sets. 
  # A student not understanding the difference between union and 
  # intersection would miss this. 
  d2 <- allElements
  
  
  # Distractor 3 (d3) includes the set difference of the source sets.
  #d3 <- dif
  
  
  d3 <- not(sourceSets[[1]], sourceSets[[2]])
  d3 <- c(d3, not(sourceSets[[2]], sourceSets[[1]]))
  
  #adding all disctractors to "wrongs" vector.
  #formatting as string here as well. 
  
  wrongs <- list()
  wrongs[[1]] <- formatListAsSet(d1)
  wrongs[[2]] <- formatListAsSet(d2)
  wrongs[[3]] <- formatListAsSet(d3)
 
  

  #format source sets as strings
  counter <- 1
  for(s in sourceSets){
    current <- formatListAsSet(s)
    sourceSets[counter] <- current
    counter <- counter + 1
  }

  #format answer as string
  answer <- formatListAsSet(answer)

  #inserting string formatting "Let A = ...."
  sourceSets <- insertSetQStrings(sourceSets)
  
  #The actual question being asked of the sets. 
  questionStr <- "Let A and B be two sets. What is \\$A\\cap B\\$?"
  
  
  #combining the source sets and question string. 
  sourceSets <- c(questionStr, sourceSets)

  
  
  #format answers and sources into json and return results 
  toSend <- list(content= sourceSets, correct= answer, distractors= wrongs)
  
  
  
  return(toSend)
  
}





# getAsymDiff(n, m) genereates and prepares n sets
# of m integers, 3 "distractors" and 1 correct answer
# when considering the difference of the generated
# sets.
# 
# NOTE: the results reflect A-B where A is the first set in
# 'source' and B is the second set in 'source'

# @param    n       The number of sets to consider
# @param    m       The number of elements in each set. 
# @response json    A json object containing the
#                   sets, correct, and incorrect
#                   answers.

getAsymDiffMC <- function(n=2, m=5, dType = 1) {
  
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
  
  answer <- set::not(sourceSets[[1]], sourceSets[[2]])
  
  
  # Distractor 1 (d1) is the difference of A-B and the difference B-A
  d1 <- c(answer, set::not(sourceSets[[2]], sourceSets[[1]]))

  # Distractor 2 (d2) is the difference B-A (the correct is A-B)
  d2 <- set::not(sourceSets[[2]], sourceSets[[1]])

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
  

  #format wrong answers as strings
  counter <- 1
  for (w in wrongs){
    current <- formatListAsSet(w)
    wrongs[counter] <- current
    counter <- counter + 1
  }
  
 #format source sets as strings
  counter <- 1
  for(s in sourceSets){
    current <- formatListAsSet(s)
    sourceSets[counter] <- current
    counter <- counter + 1
  }

  answer <- formatListAsSet(answer)

  #inserting string formatting "Let A = ...."
  sourceSets <- insertSetQStrings(sourceSets)
  
  #The actual question being asked of the sets. 
  questionStr <- "Let A and B be two sets. What is A-B?"
  
  #combining the source sets and question string.
  sourceSets <- c(questionStr, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= sourceSets, correct= answer, distractors= wrongs)
  
  
  return(toSend)
  
}

# getSetCompliment() generates and prepares n sets of m integers 
# as well as 3 "distractors" and 1 correct answer.
# The correct answer relfects the compliment of the n sets against the
# set that ranges from 0 to 20.
# 


#* @param  n        The number of sets to consider
#* @param  m        The number of elements in each set. 
#* @response json   A json object containing the
#                   sets, correct, and incorrect
#                   answers.

getSetComplementMC <- function(n=1, m = 5, dType = 1) {
  
  sourceSet <- list()    #creating an empty list.  
  universalSet <- list(1,2,3,4,5,6,7,8,9) #universal set
  
  #for each in set 1, fill the set with ints (1:9)
  sourceSet <- sample(1:9, m, replace = F)
  
  allElements <- vector()
  
  print(sourceSet)
  anwser <- not(sourceSet, universalSet)
  
  #distractor 1 is the intersection of the sets which is the wrong answer
  d1 <- intersect(sourceSet, universalSet)
  
  #distractor 2 is the intersection and the complement together which is wrong
  d2 <- universalSet
  
  #distractor 3 is the original set which is not the complement and is wrong
  d3 <- sourceSet
  
  wrongs <- list()
  wrongs[[1]] <- d1
  wrongs[[2]] <- d2
  wrongs[[3]] <- d3
  
  #format wrong answers as strings
  counter <- 1
  for (w in wrongs){
    current <- formatListAsSet(w)
    wrongs[counter] <- current
    counter <- counter + 1
  }
  
  #format source set as string
  current <- formatListAsSet(sourceSet)
  
  
  answer <- formatListAsSet(answer)
  
  #inserting string formatting "Let A = ...."
  sourceSet <- insertSetQStrings(sourceSet)
  
  #The actual question being asked of the sets. 
  questionStr <- "Let A be a single set. What is the complement of set A?"
  
  #combining the source sets and question string.
  sourceSet <- c(questionStr, sourceSet)
  
  #format answers and sources into json and return results 
  toSend <- list(content= sourceSet, correct= answer, distractors= wrongs)
  
  
  return(toSend)
  
}

getSetEqualityMC <- function(n=2, m = 5, dType = 1) {
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
  
  #using setdiff function from library
  differences <- setdiff(sourceSets[[1]], sourceSets[[2]])
  
  if(length(differences) > 0) {
    answer <- "Not Equal"
    wrong <- "Equal"
  }
  else {
    answer <- "Equal"
    wrong <- "Not Equal"
  }
  
  answer <- formatListAsSet(answer)
  
  #inserting string formatting "Let A = ...."
  sourceSets <- insertSetQStrings(sourceSets)
  
  #The actual question being asked of the sets. 
  questionStr <- "Let A and B be two sets. Are A and B equal?"
  
  #combining the source sets and question string.
  sourceSets <- c(questionStr, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= sourceSets, correct= answer, distractors= wrongs)
  
  
  return(toSend)
}

getSetCardinalityMC <- function(n=1, m = 5, dType = 1) {
  n <- 1 
  m <- sample(1:9, 1)
  sourceSet <- list() #creates an empty list
  
  sourceSet <- sample(1:9, m, replace = F)
  
  allElements <- vector()
  
  print(sourceSet)
  
  #using cardinality operation from nsprcomp library
  anwser <- cardinality(sourceSet)
  
  d1 <- cardinality(sourceSet) - 1
  
  d2 <- cardinality(sourceSet) + 1
  
  d3 <- 0
  
  wrongs <- list()
  wrongs[[1]] <- d1
  wrongs[[2]] <- d2
  wrongs[[3]] <- d3
  
  #format wrong answers as strings
  counter <- 1
  for (w in wrongs){
    current <- formatListAsSet(w)
    wrongs[counter] <- current
    counter <- counter + 1
  }
  
  #format source set as string
  current <- formatListAsSet(sourceSet)
  
  
  answer <- formatListAsSet(answer)
  
  #inserting string formatting "Let A = ...."
  sourceSet <- insertSetQStrings(sourceSet)
  
  #The actual question being asked of the sets. 
  questionStr <- "Let A be a single set. What is the cardinality of set A?"
  
  #combining the source sets and question string.
  sourceSet <- c(questionStr, sourceSet)
  
  #format answers and sources into json and return results 
  toSend <- list(content= sourceSet, correct= answer, distractors= wrongs)
  
  
  return(toSend)
}
  

