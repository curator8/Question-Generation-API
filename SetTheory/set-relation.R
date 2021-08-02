# Author:         Trevor Strobel, Joel Montano, Christopher A. Wright
# File:           set-Relation.R


library(set)
library(nsprcomp)
library(sets)
library(rje)


source("utils/format.R") #set string formatting
source("utils/set-generation.R") #set generation


# This file describes individual problems related to Set Theory




# getSetUnionMC() generates and prepares a number
# of sets as well as 3 "distractors" and 1 
# correct "answer" when considering the union of
# said sets. 
#
# param   numSets     The number of sets to consider
# param   setSize     The number of elements in each set. 
# param   dType       The desired data type for set elements
#                     (1: Ints, 2: Real, 3: Complex, 
#                     4: Char, 5: String, 6: Mixed)               
#
# return  toSend      A json-like object containing the
#                     sets, correct, and incorrect
#                     answers.

getSetUnionMC <- function(numSets=2, setSize=5, dType = 1, difficulty = 1) {
  
  if (difficulty == 1) {
  #define the text of the question
  questionText <-('Let A and B be two sets. What is \\$A\\cup B\\$?')
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- union(sourceSets[[1]], sourceSets[[2]])
  }
  if(difficulty == 2){
    chance <- sample(1:2, 1, replace = FALSE)
    numSets = 3
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    if (chance == 1) {
      #define the text of the question
      questionText <-('Let A, B, and C be three sets. What is A ∪ (B ∪ C)?')
      first <- union(sourceSets[[2]], sourceSets[[3]])
      correct <- union(first, sourceSets[[1]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
    if (chance == 2) {
      #define the text of the question
      questionText <-('Let A, B, and C be three sets. What is (A ∪ B) ∪ C?')
      first <- union(sourceSets[[1]], sourceSets[[2]])
      correct <- union(first, sourceSets[[3]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
  }
  
  
  if (difficulty > 2) {
    probability <- sample(1:2, 1, replace = FALSE)
    sourceSets <- vector(mode = "list", length = 3)
    Firstsource <- vector(mode = "list", length = 2)
    Secondsource <- vector(mode = "list", length = 2)
    Thirdsource <- vector(mode = "list", length = 2)
    Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:3, 1, replace = FALSE), 
                                   rightBorder = sample(4:6, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
    Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(7:9, 1, replace = FALSE), 
                                    rightBorder = sample(10:13, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
    Thirdsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(14:16, 1, replace = FALSE), 
                                    rightBorder = sample(17:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
    sourceSets[[1]] <- Firstsource[[2]]
    sourceSets[[2]] <- Secondsource[[2]]
    sourceSets[[3]] <- Thirdsource[[2]]
    
    if (probability == 1) {
      #define the text of the question
      questionText <-('Let A, B, and C be three sets. What is A ∪ (B ∪ C)?')
      first <- union(Secondsource[[1]], Thirdsource[[1]])
      correct <- union(first, Firstsource[[1]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
    if (probability == 2) {
      #define the text of the question
      questionText <-('Let A, B, and C be three sets. What is (A ∪ B) ∪ C?')
      first <- union(Firstsource[[1]], Secondsource[[1]])
      correct <- union(first, Thirdsource[[1]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
  }
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  # add distractors to the list. 
  # each element of the list should be a set
  # represented as a list.
  
  for(i in (1:3)){
    # generate a set
    currentDist <- correct
    if(difficulty > 1){ #difficulty higher than 1 scrambles lists in output.
      currentDist <- sample(correct, replace = FALSE)
    }
    
    if(i == 1){ #alter answer by removing an element
      currentDist <- list(currentDist[-(setSize-1)])
    }
    else if(i ==2){ #add an element to the correct answer
      # the issue here is that the "incorrect" element needs to be believable and 
      # also not possible to be in the source sets. 
      currentDist <- list(c(currentDist, getValue(x=dType)))
    }
    else if(i == 3){ #remove another element
      currentDist <- list(currentDist[-(setSize-2)])
    }

    
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  
  #formatting for output
  correct <- formatListAsSet(correct) #format for output
  
  if (difficulty < 3) {
  #Iterate through the sourceSets. format list as Set and insert at the index.
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  }
  if (difficulty == 1) {
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  }
  
  if (difficulty > 1) {
    sourceSets <- insertSet3Strings(sourceSets)
  }
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
  
}




# getSetIntersect() generates and prepares n sets
# of m elements, 3 "distractors" and 1 correct answer
# when considering the intersections of said sets.
#
# param   numSets     The number of sets to consider
# param   setSize     The number of elements in each set. 
# param   dType       The desired data type for set elements
#                     (1: Ints, 2: Real, 3: Complex, 
#                     4: Char, 5: String, 6: Mixed)               
#
# return  toSend      A json-like object containing the
#                     sets, correct, and incorrect
#                     answers.

getSetIntersectMC <- function(numSets=2, setSize=5, dType = 1, difficulty =1 ) {
  
  #define the text of the question
  questionText <-('Let A and B be two sets. What is \\$A\\cap B\\$?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- intersect(sourceSets[[1]], sourceSets[[2]])
  if(difficulty > 1){
    correct <- sample(correct, length(correct), replace = FALSE)
  }
  if(length(correct) > 0){
    correct <- formatListAsSet(correct) #format for output
  } else {
    correct <- "\\$\\emptyset\\$"
  }
   
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  #add distractors to the list. 
  for(i in (1:3)){
    currentDist <- correct
    if(difficulty > 1){ #difficulty higher than 1 scrambles lists in output.
      currentDist <- sample(correct, replace = FALSE)
    }

    if(i == 1){ #alter answer by removing an element
      if(currentDist == "\\$\\emptyset\\$"){
        currentDist <- not(sourceSets[[1]], sourceSets[[2]])
        currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
        
      } else {
        currentDist <- "\\$\\emptyset\\$"
      }
    }
    else if(i ==2){ 
      currentDist <- list(not(sourceSets[[1]], sourceSets[[2]]))
      currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
      
    }
    else if(i == 3){ #remove another element
      currentDist <- list(union(sourceSets[[1]], sourceSets[[2]]))
      currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    }
    
    
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #format answers and sources into json and return results 
  toSend <- list(content= questionContents, correct= correct, distractors= distractors)
  
  return(toSend)
}





# getAsymDiff(n, m) genereates and prepares n sets
# of m integers, 3 "distractors" and 1 correct answer
# when considering the difference of the generated
# sets.
# 
# NOTE: the results reflect A-B where A is the first set in
# 'source' and B is the second set in 'source'

# param   numSets     The number of sets to consider
# param   setSize     The number of elements in each set. 
# param   dType       The desired data type for set elements
#                     (1: Ints, 2: Real, 3: Complex, 
#                     4: Char, 5: String, 6: Mixed)               
#
# return  toSend      A json-like object containing the
#                     sets, correct, and incorrect
#                     answers.

getAsymDiffMC <- function(numSets=2, setSize=5, dType = 1, difficulty = 1) {
  if (difficulty == 1) {
  questionStr <- "Let A and B be two sets. What is A - B?"
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- not(sourceSets[[1]], sourceSets[[2]])
  
  if(length(correct) > 0){
    correct <- correct #format for output
  } else {
    correct <- "\\$\\emptyset\\$"
  }
  }
  if (difficulty == 2) {
    chance <- sample(1:2, 1, replace = FALSE)
    numSets = 3
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    if (chance == 1) {
      #define the text of the question
      questionStr <-('Let A, B, and C be three sets. What is A - (B - C)?')
      first <- not(sourceSets[[2]], sourceSets[[3]])
      correct <- not(sourceSets[[1]], first)
      correct <- sample(correct, length(correct), replace = FALSE)
    }
    if (chance == 2) {
      #define the text of the question
      questionStr <-('Let A, B, and C be three sets. What is (A - B) - C?')
      first <- not(sourceSets[[1]], sourceSets[[2]])
      correct <- not(first, sourceSets[[3]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
  }
  
  if (difficulty == 3) {
    probability <- sample(1:2, 1, replace = FALSE)
    sourceSets <- vector(mode = "list", length = 3)
    Firstsource <- vector(mode = "list", length = 2)
    Secondsource <- vector(mode = "list", length = 2)
    Thirdsource <- vector(mode = "list", length = 2)
    Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:3, 1, replace = FALSE), 
                                   rightBorder = sample(5:6, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
    Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(8:9, 1, replace = FALSE), 
                                    rightBorder = sample(11:13, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
    Thirdsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(15:16, 1, replace = FALSE), 
                                   rightBorder = sample(18:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
    sourceSets[[1]] <- Firstsource[[2]]
    sourceSets[[2]] <- Secondsource[[2]]
    sourceSets[[3]] <- Thirdsource[[2]]
    
    if (probability == 1) {
      #define the text of the question
      questionStr <-('Let A, B, and C be three sets. What is A - (B - C)?')
      first <- not(Secondsource[[1]], Thirdsource[[1]])
      correct <- not(first, Firstsource[[1]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
    if (probability == 2) {
      #define the text of the question
      questionStr <-('Let A, B, and C be three sets. What is (A - B) - C?')
      first <- not(Firstsource[[1]], Secondsource[[1]])
      correct <- not(first, Thirdsource[[1]])
      correct <- sample(correct, length(correct), replace = FALSE)
    }
  }
  
  #Create the distractors
  distractors <- vector(mode="list", length = 3)
  
  #add distractors to the list. 
  for(i in (1:3)){
    # generate a set
    currentDist <- correct
    
    if(i == 1){ #alter answer by removing an element
      currentDist <- list(currentDist[-(setSize-1)])
    }
    else if(i ==2){ #add an element to the correct answer
      # the issue here is that the "incorrect" element needs to be believable and 
      # also not possible to be in the source sets. 
      currentDist <- list(c(currentDist, getValue(x=dType)))
    }
    else if(i == 3){ #remove another element
      currentDist <- list(currentDist[-(setSize-2)])
    }
    
    
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  correct <- formatListAsSet(correct)
  if (difficulty < 3) {
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  }
  
  if (difficulty == 1) {
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  }
  else {
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  # "C = {...}"
  sourceSets <- insertSet3Strings(sourceSets)
  }
  # now we concatenate the question contents together
  questionContents <- c(questionStr, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  return(toSend)
  
}

# getSetCompliment() generates and prepares n sets of m members 
# as well as 3 "distractors" and 1 correct answer.
# The correct answer reflects the compliment of the n sets against the
# universal set.
# 
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#
getSetComplementMC <- function(numSets = 2, setSize = 9, dType = 1, difficulty =1) {
  
  questionText <- "Let A be a set and B be the universal set. What is the complement of set A?"
  if (difficulty < 3) {
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  sourceSets[[1]] <- sourceSets[[2]]
  #scramble Universal set
  sourceSets[[2]] <- sample(sourceSets[[2]], length(sourceSets[[2]]), replace  = FALSE)
  length(sourceSets[[1]]) <- 5
  
  correct <- not(sourceSets[[2]], sourceSets[[1]])

  #shuffles when diff over 1
  if(difficulty > 1){
    correct <- sample(correct, length(correct), replace = FALSE)
  }
  }
  
  if (difficulty > 2) {
    sourceSets <- vector(mode = "list", length = 2)
    Firstsource <- vector(mode = "list", length = 2)
    Secondsource <- vector(mode = "list", length = 2)
    Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(5:10, 1, replace = FALSE), 
                                   rightBorder = sample(11:15, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
    Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:4, 1, replace = FALSE), 
                                    rightBorder = sample(16:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
    sourceSets[[1]] <- Firstsource[[2]]
    sourceSets[[2]] <- Secondsource[[2]]
    correct <- not(Secondsource[[1]], Firstsource[[1]])
  }
  
  d1 <- correct
  d2 <- correct
  correct <- formatListAsSet(correct)
  
  distractors <- vector(mode="list", length = 3)
  
  #distractor 1 Is similar to the correct answer, but with one different value
  d1 <- replace(d1, length(d1) - 2, getValue(x = dType, min = 1, max = 20)) 
  if(difficulty > 1){
    d1 <- sample(d1, replace= FALSE)
  }
  #distractor 2 is also similar to the correct answer, but with one replaced value
  d2 <- replace(d2, length(d2), getValue(x = dType, min = 1, max = 20))
  if(difficulty > 1){
    d2 <- sample(d2, replace= FALSE)
  }
  #distractor 3 is the original set which is not the complement and is wrong
  d3 <- sourceSets[[1]]
  if(difficulty > 1){
    d3 <- sample(d3, replace= FALSE)
  }
  
  distractors[[1]] <- formatListAsSet(d1)
  distractors[[2]] <- formatListAsSet(d2)
  if (difficulty < 3) {
    distractors[[3]] <- formatListAsSet(d3)
    #Iterate through the sourceSets. format list as Set and insert at the index.
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
  }
  }
  if (difficulty == 3) {
    distractors[[3]] <- d3
  }
  #format the the sourceSets as Question Strings
  # "A = {...}"
  # "B = {...}"
  sourceSets <- insertSetQStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}

  
# getSetEquality() generates and prepares 2 sets of m members 
# as well as 1 "distractor" and 1 correct answer.
# The correct answer is a string which states whether the sets are equal
# or not.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#

getSetEqualityMC <- function(numSets = 2, setSize = 5, dType = 1, difficulty = 1) {
  probability <- sample(1:2, 1, replace = FALSE)
  if (difficulty == 1) {
    questionText <- "Let A and B be two sets. Are A and B equal?"
    #Hard Coded this as the function only works with two sets at the moment.
    numSets <- 2
    #sets 50/50 probability of generated sets being equal or not.
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    if (probability == 1) {
      #makes 2nd set equal to first and formats correct and incorrect answers.
      sourceSets[[2]] <- sourceSets[[1]]
      correct <- "Equal" #format for output
      distractors <- "Not Equal"
    }
    if (probability == 2) {
      #makes 2nd set equal to first except for one replaced member, and formats answers.
      sourceSets[[2]] <- sourceSets[[1]]
      sourceSets[[2]] <- replace(sourceSets[[2]], 
      length(sourceSets[[2]]) - sample(1:4, 1, replace = FALSE), 
      getValue(x = dType, min = 1, max = 20, cat = 6))
      correct <- "Not Equal"
      distractors <- "Equal"
      
    }
    #Iterate through the sourceSets. format list as Set and insert at the index.
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
    
  }
  
  if (difficulty == 2) {
    
    # Changes to a different question from difficulty 1. Asks to find the equivalent set.
    questionText <- "Let A be a set. Which set is equivalent to set A?"
    numSets <- 1
    leftBorder <- sample(2:5, 1, replace = FALSE)
    rightBorder <- sample(7:10, 1, replace = FALSE)
    
    # Generates sourceSet with setNotations function, and alters notation of correct answer
    # so that it is always different than the notation of the sourceSet for a more challenging question.
    sourceSets <- vector(mode = "list", 2)
    answer <- vector(mode = "list", 2)
    notation <- sample(2:3, 1, replace = FALSE)
    sourceSets <- getSetNotations(leftIncl = TRUE, rightIncl = TRUE, leftBorder, 
                                  rightBorder, membersType = 1, notation)
    if (notation == 2) {
      answer <- getSetNotations(leftIncl = FALSE, rightIncl = FALSE, leftBorder - 1, 
                                 rightBorder + 1, membersType = 1, notation = 1)
      correct <- answer[[2]]
    }
    else if (notation == 3){
      answer <- getSetNotations(leftIncl = TRUE, rightIncl = FALSE, leftBorder, 
                                rightBorder + 1, membersType = 1, notation = 1)
      correct <- answer[[2]]
    }
    else {
      answer <- getSetNotations(leftIncl = FALSE, rightIncl = TRUE, leftBorder - 1, 
                                 rightBorder, membersType = 1, notation = 1)
      correct <- answer[[2]]
    }
  }   
    if (difficulty > 2) {
      questionText <- "Let A be a set. Which set is not equivalent to set A?"
      
      numSets <- 1
      leftBorder <- sample(2:5, 1, replace = FALSE)
      rightBorder <- sample(7:10, 1, replace = FALSE)
      leftIncl <- TRUE
      rightIncl <- TRUE
      # Generates sourceSet with setNotations function, and alters notation of correct answer
      # so that it is always different than the notation of the sourceSet for a more challenging question.
      sourceSets <- vector(mode = "list", 2)
      answer <- vector(mode = "list", 2)
      probability <- sample(1:3, 1, replace = FALSE)
      
      sourceSets <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                                    rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
      if (probability == 2) {
        answer <- getSetNotations(leftIncl, rightIncl, leftBorder - 1, 
                                  rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
        correct <- answer[[2]]
      }
      else if (probability == 3){
        answer <- getSetNotations(leftIncl, rightIncl, leftBorder + 1, 
                                  rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
        correct <- answer[[2]]
      }
      else {
        answer <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                                  rightBorder - 1, membersType = 1, notation = sample(1:3, 1, replace = FALSE))
        correct <- answer[[2]]
      }
    }
    if (difficulty > 1) {
    #Create the distractors
    distractors <- vector(mode="list", length = 3)
    
    for(i in (1:3)){
      # Generates distractors with different included or excluded borders to make them
      # different from the correct answer.
      if (difficulty == 2) {
        d1 <- getSetNotations(leftIncl = TRUE, rightIncl = FALSE, leftBorder, 
                              rightBorder, membersType = 1, notation = 1)
        d2 <- getSetNotations(leftIncl = FALSE, rightIncl = FALSE, leftBorder, 
                              rightBorder, membersType = 1, notation = 1)
        d3 <- getSetNotations(leftIncl = FALSE, rightIncl = TRUE, leftBorder, 
                              rightBorder, membersType = 1, notation = 1)
      }
      else if (difficulty > 2) {
        d1 <- getSetNotations(leftIncl = TRUE, rightIncl = FALSE, leftBorder, 
                              rightBorder + 1, membersType = 1, notation = 2)
        d2 <- getSetNotations(leftIncl = FALSE, rightIncl = FALSE, leftBorder - 1, 
                              rightBorder + 1, membersType = 1, notation = 3)
        d3 <- getSetNotations(leftIncl = FALSE, rightIncl = TRUE, leftBorder - 1, 
                              rightBorder, membersType = 1, notation = 1)
      }
      distractors[[1]] <- d1[[2]]
      distractors[[2]] <- d2[[2]]
      distractors[[3]] <- d3[[2]]
    }
   
    }

  if (difficulty == 1) {
    #format the the sourceSets as Question Strings
    # "A = {...}"
    # "B = {...}"
    sourceSets <- insertSetQStrings(sourceSets)
  }
  else {
    #format the the sourceSets as Question Strings
    # "A = {...}"
    sourceSets <- insertSetRStrings(sourceSets[[2]])
  }
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}

# getSetCardinality() generates and prepares 1 set of a random number of 
# members between 1 and 9, as well as 3 "distractors" and 1 correct answer.
# The correct answer is a string which states the correct cardinality
# of the generated set.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#
getSetCardinalityMC <- function(numSets = 1, setSize = sample(1:9, 1, replace = FALSE), dType = 1, difficulty = 1) {
  #define the text of the question
  questionText <-('Let A be a set. What is the cardinality of set A?')
  if (difficulty == 1) {
    #generate and fill sets
    sourceSet <- getSets(n = numSets, m = setSize, x = dType)
  }
  
  if (difficulty == 2) {
    setSize = 9
    #generate and fill sets
    sourceSet <- getSets(n = numSets, m = setSize, x = dType)
    initial <- sourceSet[[1]]
    
    # Creating a probability variable which will cause the correct answer to be 
    # generated with one of three different partitions for question variety.
    # Setting length of initial variable creates the first partition.
    length(initial) <- sample(0:4, 1, replace = FALSE)
    
    # Then creates the second partition with the remaining members from the source.
    secondSet <- not(sourceSet[[1]], initial)
    
    # Formats each inner partition as a set and concatenates each set
    # within the larger list. Then formats the larger list as a set.
    if (sample(1:2, 1, replace = FALSE) == 2) {
      initial <- formatPartitionAsSet(initial)
    }
    sourceSet[[1]] <- list()
    sourceSet[[1]] <- c(sourceSet[[1]], initial)
    
    #chance variable will decide whether there are two or three partitioned sets
    chance <- sample(1:2, 1, replace = FALSE)
    if (chance == 1) {
      secondSet <- formatPartitionAsSet(secondSet)
      sourceSet[[1]] <- c(sourceSet[[1]], secondSet)
    }
    if (chance == 2) {
      secondPartition <- secondSet
      length(secondPartition) <- sample(0:4, 1, replace = FALSE)
      thirdPartition <- not(secondSet, secondPartition)
      secondSet <- formatPartitionAsSet(secondSet)
      if (sample(1:2, 1, replace = FALSE) == 2) {
        secondPartition <- formatPartitionAsSet(secondPartition)
      }
      if (sample(1:2, 1, replace = FALSE) == 2) {
        thirdPartition <- formatPartitionAsSet(thirdPartition)
      }
      sourceSet[[1]] <- c(sourceSet[[1]], secondPartition)
      sourceSet[[1]] <- c(sourceSet[[1]], thirdPartition)
    }
  }
  
  if (difficulty > 2) {
    probability <- sample(1:2, 1, replace = FALSE)
    leftIncl <- sample(c(TRUE,FALSE), 1, replace = FALSE)
    rightIncl <- sample(c(TRUE,FALSE), 1, replace = FALSE)
    sourceSets <- vector(mode = "list", length = 2)
    Firstsource <- vector(mode = "list", length = 2)
    Secondsource <- vector(mode = "list", length = 2)
    if (probability == 1) {
      questionText <-('Let A be a set and B be a set. What is the total cardinality of sets A and B?')
      Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(5:10, 1, replace = FALSE), 
                                     rightBorder = sample(11:15, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
      Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(1:4, 1, replace = FALSE), 
                                      rightBorder = sample(16:20, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
      sourceSets[[1]] <- Firstsource[[2]]
      sourceSets[[2]] <- Secondsource[[2]]
      correct <- lengths(Firstsource[1]) + lengths(Secondsource[1])
    }
    if (probability == 2) {
      questionText <-('Let A be a set and B be a set. Based on the relative cardinalities of A and B, what type of function is A → B?')
      Firstsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(6:7, 1, replace = FALSE), 
                                     rightBorder = sample(15:16, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE) 
      Secondsource <- getSetNotations(leftIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), rightIncl = sample(c(TRUE, FALSE), 1, replace = FALSE), leftBorder = sample(6:7, 1, replace = FALSE), 
                                      rightBorder = sample(15:16, 1, replace = FALSE), membersType = 1, notation = sample(2:3, 1, replace = FALSE), format = TRUE)
      sourceSets[[1]] <- Firstsource[[2]]
      sourceSets[[2]] <- Secondsource[[2]]
      distractors <- vector(mode="list", length = 2)
      if ((lengths(Firstsource[1])) == (lengths(Secondsource[1]))) {
        correct <- "Bijection"
        distractors[[1]] <- "Surjection"
        distractors[[2]] <- "Injection"
      }
      else if ((lengths(Firstsource[1])) < (lengths(Secondsource[1]))) {
        correct <- "Injection"
        distractors[[1]] <- "Surjection"
        distractors[[2]] <- "Bijection"
      }
      else {
        correct <- "Surjection"
        distractors[[1]] <- "Bijection"
        distractors[[2]] <- "Injection"
      }
    }
  }
  
  if (difficulty < 3) {
    #creating the correct answer based on length of SourceSet
    correct <- lengths(sourceSet) 
  }
  
  if (difficulty < 3 || probability == 1) {
    #Creating distractors based on correct answer.
    distractors <- vector(mode="list", length = 3)
    probability <- sample(1:2, 1, replace = FALSE)
    for(i in 1:3) {
      if (probability == 1) {
          distractors[[i]] <- correct - sample(1:3, 1, replace = FALSE)
      }
      if (probability == 2) {
          distractors[[i]] <- correct + sample(1:3, 1, replace = FALSE)
      }
  }
  }
 
  #Iterate through the sourceSet. format list as Set and insert at the index.
  if (difficulty < 3) {
    counter <- 1
    for (s in sourceSet){
      sourceSet[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
    #format the the sourceSet as Question Strings
    # "A = {...}"
    sourceSets <- insertSetRStrings(sourceSet)
  }
  
  if (difficulty > 2) {
    #format the the sourceSet as Question Strings
    # "A = {...}"
    # "B = {...}"
    sourceSets <- insertSetQStrings(sourceSets)
  }
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}

# getSymmDiff() generates and prepares 2 sets of length 5, 
# as well as 3 "distractors" and 1 correct answer.
# The correct answer is a string which states the unique members of each set
# which constitute the symmetric difference between the two sets.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#
  
getSymmDiffMC <- function(numSets = 2, setSize = 5, dType = 1, difficulty = 1){
  
  #define the text of the question
  questionText <-('Let A and B be two sets. What is the symmetric difference of A and B?')
  
  if (difficulty == 1) {
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
    #set sourceSet 2 equal to sourceSet 1 and scramble the set, then replace three members.
    sourceSets[[2]] <- sourceSets[[1]]
  
    sourceSets[[2]] <- sample(sourceSets[[2]], length(sourceSets[[2]]), replace  = FALSE)
  
    sourceSets[[2]] <- replace(sourceSets[[2]], length(sourceSets[[2]]) - sample(0:1, 1, replace = FALSE), 
                             getValue(x = dType, min = 21, max = 30, cat = 6))
    sourceSets[[2]] <- replace(sourceSets[[2]], length(sourceSets[[2]]) - sample(0:1, 1, replace = FALSE), 
                             getValue(x = dType, min = 21, max = 30, cat = 6))
    sourceSets[[2]] <- replace(sourceSets[[2]], length(sourceSets[[2]]) - sample(0:1, 1, replace = FALSE), 
                             getValue(x = dType, min = 21, max = 30, cat = 6))
  
  
    #set correct answer as a list of values unique to both sets
    correct <- list()
    correct <- not(sourceSets[[1]], sourceSets[[2]])
    correct <- append(correct, not(sourceSets[[2]], sourceSets[[1]]), after = length(correct))
  
    #Iterate through the sourceSets. format list as Set and insert at the index.
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
  }
  
  if (difficulty > 1) {
    #Creates two partitions within a larger set based on the differences between the
    # borders of the sourceSet.
    if (difficulty == 2) {
      sourceSets <- vector(mode = "list", 2)
      SetOne <- list()
      SetTwo <- list()
    }
    if (difficulty > 2) {
      #define the text of the question
      questionText <-('Let A, B, and C be three sets. What is the symmetric difference of A, B, and C?')
      sourceSets <- vector(mode = "list", 3)
      SetOne <- list()
      SetTwo <- list()
      SetThree <- list()
    }
    leftIncl <- TRUE
    rightIncl <- TRUE
    # Difference variable defines the random difference in both borders between the 1st and 2nd
    # sourceSets so there will always be a symmetric difference.
    difference <- sample(1:2, 1, replace = FALSE)
    leftdifference <- list()
    rightdifference <- list()
    leftBorder <- sample(7:8, 1, replace = FALSE)
    rightBorder <- sample(9:10, 1, replace = FALSE)
    SetOne <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                              rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = FALSE)
    SetTwo <- getSetNotations(leftIncl, rightIncl, leftBorder + difference, 
                              rightBorder + difference, membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = FALSE)
    if (difficulty > 2) {
      SetThree <- getSetNotations(leftIncl, rightIncl, leftBorder + (difference * 2), 
                                            rightBorder + (difference * 2), membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = FALSE)
    }
    sourceSets[[1]] <- SetOne[[2]]
    sourceSets[[2]] <- SetTwo[[2]]
    if (difficulty > 2) {
      sourceSets[[3]] <- SetThree[[2]]
    }
    # Creates partitions containing the symmetric differences of both sets
    # and appends them to the correct list
    leftdifference <- getSetNotations(leftIncl, rightIncl, leftBorder, 
                                      leftBorder + difference - 1, membersType = 1, notation = 1, format = FALSE)
    rightdifference <- getSetNotations(leftIncl, rightIncl, rightBorder + 1, 
                                       rightBorder + difference, membersType = 1, notation = 1,format = FALSE )
    correct <- list()
    correct <- append(correct, leftdifference[[1]])
    correct <- append(correct, rightdifference[[1]])
    if (difficulty > 2) {
      # Creates partitions containing the symmetric differences of both sets
      # and appends them to the correct list
      rightdifferencetwo <- list()
      rightdifferencetwo <- getSetNotations(leftIncl, rightIncl, rightBorder + difference + 1, 
                                      rightBorder + difference + difference, membersType = 1, notation = 1, format = FALSE)
      correct <- append(correct, rightdifferencetwo[[1]])
    }
  }
    #Creating distractors based on correct answer.
    distractors <- vector(mode="list", length = 3)
    
    for (i in (1:3)) {
      currentDist <- list()
      currentDist[[1]] <- correct
      wrong <- currentDist[[1]]
      wrong <- replace(wrong, length(wrong) - sample(0:1, 1, replace = FALSE), getValue(x = dType, min = 1, max = 30, cat = 6))
      currentDist[[1]] <- wrong
      currentDist <- formatListAsSet(currentDist[[1]])
      distractors[i] <- currentDist
    }
    
    correct <- formatListAsSet(correct)
  
  if (difficulty < 3) {
    #format the the sourceSet as Question Strings
    # "A = {...}"
    # "B = {...}"
    sourceSets <- insertSetQStrings(sourceSets)
  }
  else {
    #format the the sourceSet as Question Strings
    # "A = {...}"
    # "B = {...}"
    # "C = {...}"
    sourceSets <- insertSet3Strings(sourceSets)
  }
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)

  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)

  #return question info
  return(toSend)
}
  
# getSetPartitions() generates and prepares 1 set of length 5, 
# as well as 3 "distractors" and 1 correct answer.
# The correct answer is the set which represents an incorrect partition
# of the sourceSet.
#
#
# @param  numSets        The number of sets to consider
# @param  setSize        The number of elements in each set. 
# @response json         A json object containing the
#                        sets, correct, and incorrect
#                        answers.
#

getSetPartitionsMC <- function(numSets = 1, setSize = 5, dType = 1, difficulty = 1) {
  
  if (difficulty == 1) {
    #define the text of the question
    questionText <-('Let A be a set. Which answer represents a correct set partition of set A?')
  }
  if (difficulty > 1) {
    #define the text of the question
    questionText <-('Let A be a set. Which answer represents an incorrect set partition of set A?')
  }

  if (difficulty < 3) {
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
    #scrambling the sets to be used for both the correct and distractor partitions.
    initial <- sample(sourceSets[[1]], length(sourceSets[[1]]), replace  = FALSE)
  
    correct <- list()
  
    # Creating a probability variable which will cause the correct answer to be 
    # generated with one of three different partitions for question variety.
    # Setting length of initial variable creates the first partition.
    probability <- sample(1:3, 1, replace = FALSE)
    if (probability == 1) {
      length(initial) <- 3
    }
    else if (probability == 2) {
      length(initial) <- 2
    }
    else {
      length(initial) <- 0
    }
  
    # Then creates the second partition with the remaining members from the source.
    secondSet <- not(sourceSets[[1]], initial)
  
    if (difficulty == 2) {
    # Replaces one member of the first partition with a random member, thus making
    # the partitioning incorrect and generating the correct answer choice.
    initial <- replace(initial, length(initial) - sample(0:2, 1, replace = FALSE),
    getValue(x = dType, min = 1, max = 20))
    }
  
    # Formats each inner partition as a set and concatenates each set
    # within the larger list. Then formats the larger list as a set.
    initial <- formatPartitionAsSet(initial)
    secondSet <- formatPartitionAsSet(secondSet)
    correct <- c(correct, initial)
    correct <- c(correct, secondSet)
    correct <- formatListAsSet(correct)
  }
  
  if (difficulty > 2) {
    # Define sourceSets and generate set with setNotations function.
    sourceSets <- vector(mode="list", length = 1)
    originalSet <- vector(mode="list", length = 2)
    leftBorder <- sample(10:11, 1, replace = FALSE)
    rightBorder <- sample(19:20, 1, replace = FALSE)
    PartitionLength <- sample(2:5, 1, replace = FALSE)
    originalSet <- getSetNotations(leftIncl = TRUE, rightIncl = TRUE, leftBorder, 
                                   rightBorder, membersType = 1, notation = sample(1:3, 1, replace = FALSE), format = TRUE)
    sourceSets[[1]] <- originalSet[[2]]
    # Set correct variable as list that contains an incorrect partitioning of the sourceSet.
    # Then append to the correct list and format.
    correct <- vector(mode = "list", length = 1)
    firstPartition <- getSetNotations(leftIncl = FALSE, rightIncl = TRUE, leftBorder, 
                                      rightBorder - PartitionLength, membersType = 1, notation = 1, format = FALSE)
    secondPartition <- getSetNotations(leftIncl = TRUE, rightIncl = FALSE, rightBorder - PartitionLength + 1, 
                                       rightBorder, membersType = 1, notation = 1, format = FALSE)
    correct[[1]] <- append(correct[[1]], firstPartition[[2]])
    correct[[1]] <- append(correct[[1]], secondPartition[[2]])
    correct[[1]] <- formatListAsSet(correct[[1]])
  }
  
  distractors <- vector(mode="list", length = 3)
  
  
  for(i in (1:3)) {
   if (difficulty < 3) {
     #generate and partition distractor sets
     currentDist <- (getSets(n = 1, m = 5, x = dType))
     firstSet <- sample(sourceSets[[1]], length(sourceSets[[1]]), replace  = FALSE)
     length(firstSet) <- sample(2:4, 1, replace = FALSE)
     secondSet <- not(sourceSets[[1]], firstSet)
     if (difficulty == 1) {
     firstSet <- replace(firstSet, length(firstSet) - sample(0:2, 1, replace = FALSE),
                        getValue(x = dType, min = 1, max = 20))
     }
     firstSet <- formatPartitionAsSet(firstSet)
     secondSet <- formatPartitionAsSet(secondSet)
     wrong <- list()
     # and concatenating both sets inside larger empty list.
     # Wrong variable is created to deal with weird out of bounds issue in R.
     wrong <- c(wrong, firstSet)
     wrong <- c(wrong, secondSet)
     currentDist[[1]] <- wrong
   }
   if (difficulty > 2) {
     #generate distractor partitions and notations, and append to currentDist
     currentDist <- vector(mode = "list", length = 1)
     DistractorPartitionLength <- sample(2:9, 1, replace = FALSE)
     firstDistractorPartition <-getSetNotations(leftIncl = TRUE, rightIncl = TRUE, leftBorder, 
                                                rightBorder - DistractorPartitionLength, membersType = 1, notation = 1, format = FALSE)
     secondDistractorPartition <- getSetNotations(leftIncl = TRUE, rightIncl = TRUE, rightBorder - DistractorPartitionLength + 1, 
                                                  rightBorder, membersType = 1, notation = 1, format = FALSE)
     currentDist[[1]] <- append(currentDist[[1]], firstDistractorPartition[[2]])
     currentDist[[1]] <- append(currentDist[[1]], secondDistractorPartition[[2]])
   }
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  if (difficulty < 3) {
    #Iterate through the sourceSets. format list as Set and insert at the index.
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
  }
  #format the the sourceSet as Question Strings
  # "A = {...}"
  sourceSets <- insertSetRStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  #return question info
  return(toSend)
}


# powerSetQA() generates and prepares 1 set of a random number of 
# members between 1 and 9, as well as 3 "distractors" and 1 correct answer.
# The correct answer is a string which states the correct cardinality
# of the generated set.
#   @param      numSets         The number of sets to consider in the question
#   @param      setSize         The length of the source sets.
#   @param      dType           The desired data type for set elements
#                               (1: Ints, 2: Real, 3: Complex, 
#                               4: Char, 5: String, 6: Mixed)  
#
#   @return     toSend          A json-like object containing the
#                               source sets, correct, and 
#                               distractors (incorrect answers)
#
powerSetQA <- function(numSets = 1, setSize = 3, dType = 6, difficulty = 1) {
  #level 1 difficulty only generates random sets with random data type.
  if(difficulty == 1) {
    
    #question text 
    questionText <-('What is the power set of A?')
    
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    
    
    
    #convert the set into a vector to manipulate better. 
    sourceSetsVector <- unlist(sourceSets)
    
    #how the power set is gotten (uses the set_power function from 'sets' package).
    correct <- set_power(sourceSetsVector)
    #converts power set back to a string to format. 
    correct <- toString(correct, width = NULL) 
    #uses the formatListAsSet function to format the string located in "utils/format.R".
    correct <- formatListAsSet(correct[[1]])
    
    
    
    
    #formats the power set string correctly.
    correct <- str_replace_all(correct, c("list" = ""))
    correct <- str_replace_all(correct, c("\\(" = "\\\\{"))
    correct <- str_replace_all(correct, c("\\)" = "\\\\}"))
    correct <- str_replace_all(correct, c("\"" = ""))
    
    
    
    
    
    
    
    #Create a vector that will hold distractors
    #NOTE: we declare the list with vector() here so that 
    # we can also declare a length. This prevents R from
    # copying the list every time we add an element
    distractors <- vector(mode="list", length = 3)
    
    #add distractors to the list. 
    # each element of the list should be a set
    # represented as a list.
    for(i in (1:3)){
      #generate a set using getSets function. 
      currentDist <- (getSets(n = 1, m = setSize, x = dType))
      #makes the list into a vector to better manipulate 
      currentDist <- unlist(currentDist)
      #gets the power set of the vector. 
      #currentDist <- 2^as.set(currentDist)
      currentDist <- set_power(currentDist)
      #converts the power set into a string.
      currentDist <- toString(currentDist, width = NULL) 
      #formats the power set. 
      currentDist <- formatListAsSet(currentDist[[1]])    #The [[1]] is important here as it removes a layer of abstraction imposed by R
      #formats the power set string correctly. 
      currentDist <- str_replace_all(currentDist, c("list" = ""))
      currentDist <- str_replace_all(currentDist, c("\\(" = "\\\\{"))
      currentDist <- str_replace_all(currentDist, c("\\)" = "\\\\}"))
      currentDist <- str_replace_all(currentDist, c('\"' = ""))
      
      
      #Note the single brackets '[1]' here 
      distractors[i] <- currentDist
    }
  } 
  
  
  #level 2 difficulty generates smarter distractors that are related to the correct answer and introduces questions on the cardinality of power sets.   
  else if (difficulty == 2) {
    
    #randomly chooses between traditional power set question or cardinality question. 
    randomChoice <- sample(1:2, 1)
    
    if (randomChoice == 1) {
      questionText <- ('What is the power set of A?')
      
      setSize = sample((1:4), 1)
      
      #generate and fill sets
      sourceSets <- getSets(n = numSets, m = setSize, x = dType)
      
      
      
      #convert the set into a vector to manipulate better.
      sourceSetsVector <- unlist(sourceSets)
      #how the power set is gotten (uses the 'sets' package).
      correct <- set_power(sourceSetsVector)
      #converts power set back to a string to format.
      correct <- toString(correct, width = NULL)
      #uses the formatListAsSet function to format the string located in "utils/format.R".
      correct <- formatListAsSet(correct[[1]])
      #used for distractor generation. 
      correctDisctractor <- correct
      
      
      
      #formats the power set string correctly.
      correct <- str_replace_all(correct, c("list" = ""))
      correct <- str_replace_all(correct, c("\\(" = "\\\\{"))
      correct <- str_replace_all(correct, c("\\)" = "\\\\}"))
      correct <- str_replace_all(correct, c("\"" = ""))
      
      
      
      
      
      #Create a vector that will hold distractors
      #NOTE: we declare the list with vector() here so that
      # we can also declare a length. This prevents R from
      # copying the list every time we add an element
      distractors <- vector(mode = "list", length = 3)
      
      #add distractors to the list.
      # each element of the list should be a set
      # represented as a list.
      for (i in (1:3)) {
        
        #each distractor is individually manipulated to make them more tricky.
        
        if (i == 1) {
          #using a correct set to generate better distractors.
          currentDist <- correctDisctractor
          
          #gets rid of extra fluff power_set function throws in.
          currentDist <- str_replace_all(currentDist, c("list" = ""))
          currentDist <- str_replace_all(currentDist, c('\"' = ""))
          
          
          #Note the single brackets '[1]' here
          distractors[i] <- currentDist
          
        } else if (i == 2) {
          
          
          #using a correctDistractor set mentioned earlier to generate better distractors.
          currentDist <- correctDisctractor
          
          #gets rid of extra fluff power_set function throws in and adds character to generate wrong Power set format.
          currentDist <- str_replace_all(currentDist, c("list" = ""))
          currentDist <- str_replace_all(currentDist, c("\\(\\), " = ""))
          currentDist <- str_replace_all(currentDist, c("\\(" = "\\\\{"))
          currentDist <- str_replace_all(currentDist, c("\\)" = "\\\\}"))
          currentDist <- str_replace_all(currentDist, c('\"' = ""))
          
          
          #Note the single brackets '[1]' here
          distractors[i] <- currentDist
          
        } else if (i == 3) {
          #uses an alternative power set function from the rje package that takes the same value but doesn't generate correct set. 
          currentDist <- powerSetCond(sourceSetsVector, m = sample(1:setSize, 1),rev = FALSE)
          currentDist <- toString(currentDist,  width = NULL)
          
          #uses the formatListAsSet function to format the string located in "utils/format.R".
          currentDist <- formatListAsSet(currentDist)
          
          
          
          
          currentDist <- str_replace_all(currentDist, c("list" = ""))
          currentDist <- str_replace_all(currentDist, c("\\(" = "\\\\{"))
          currentDist <- str_replace_all(currentDist, c("\\)" = "\\\\}"))
          currentDist <- str_replace_all(currentDist, c('\"' = ""))
          currentDist <- str_replace_all(currentDist, c(':' = ", "))
          currentDist <- str_replace_all(currentDist, c('c' = ""))
          
          
          
          #Note the single brackets '[1]' here
          distractors[i] <- currentDist
          
        }
      }
      
    } else {
      
      #questionAsked randomly chooses between two ways to ask a cardinality question involving a power set. 
      questionAsked <- sample(1:2, 1)
      
      if (questionAsked == 1) {
        questionText <- ('What is the cardinality of the power set of A?')
      } else {
        questionText <- ('What is |P(A)|?')
      }
      
      #used to determine how the answer and distractors will be formatted. 
      formatGenerated <- sample(1:2, 1)
      
      #generates a set size from 1 to 4 long.
      setSize = sample((1:4), 1)
      
      #generate and fill sets
      sourceSets <- getSets(n = numSets, m = setSize, x = dType)
      
      
      #convert the set into a vector to manipulate better.
      sourceSetsVector <- unlist(sourceSets)
      
      
      #if formatGenerated is 1 it will generate the integer format of the cardinality.
      if (formatGenerated == 1) {
        
        #uses the set_power function from 'sets' package.
        correct <- set_power(sourceSetsVector)
        
        
        #the correct cardinality of the power set.
        correct <- length(correct)
        
        #used later for distractor generation.
        correctDisctractor <- correct
        
        #converts power set back to a string to format. This is one of the 2 formats
        correct <- toString(correct, width = NULL)
        
        
        #formats the power set string correctly.
        correct <- str_replace_all(correct, c("list" = ""))
        correct <- str_replace_all(correct, c("\\(" = "\\\\{"))
        correct <- str_replace_all(correct, c("\\)" = "\\\\}"))
        correct <- str_replace_all(correct, c("\"" = ""))
        
        
        
        
        
        
        #Create a vector that will hold distractors
        #NOTE: we declare the list with vector() here so that
        # we can also declare a length. This prevents R from
        # copying the list every time we add an element
        distractors <- vector(mode = "list", length = 3)
        
        #add distractors to the list.
        # each element of the list should be a set
        # represented as a list.
        for (i in (1:3)) {
          
          #individually manipulate the distractors to make them trickier. 
          if (i == 1) {
            
            #get the length of the original set to coumpound by 3. 
            currentDist <- 3 ** length(sourceSetsVector)
            currentDist <- toString(currentDist, width = NULL)
            
            
            #Note the single brackets '[1]' here
            distractors[i] <- currentDist
            
          } else if (i == 2) {
            
            #using the length of the original set 'A" to generate a better distractors. 
            if (length(sourceSetsVector) != 1 & length(sourceSetsVector) != 2) {
              currentDist <- 2 * length(sourceSetsVector)
              currentDist <- toString(currentDist, width = NULL)
            } else {
              currentDist <- 10 * length(sourceSetsVector)
              currentDist <- toString(currentDist, width = NULL)
              
            }
            
            
            #Note the single brackets '[1]' here
            distractors[i] <- currentDist
            
          } else if (i == 3) {
            
            #generates a random cardinality not relevant to correct. 
            currentDist <- sample((correctDisctractor + 1):(correctDisctractor + 5), 1)
            currentDist <- toString(currentDist, width = NULL)
            
            
            #Note the single brackets '[1]' here
            distractors[i] <- currentDist
            
          }
          
        }
        
        
        
      } else {
        
        #second way the cardinality answers is formatted,which uses 2^length instead of integer format.
        stringedAnswer <-toString(length(sourceSetsVector), width = NULL)
        correct <- paste("\\$", "2^", stringedAnswer, "\\$")
        
        
        #used later for distractor generation.
        correctDisctractor <- length(set_power(sourceSetsVector))
        
        
        #Create a vector that will hold distractors
        #NOTE: we declare the list with vector() here so that
        # we can also declare a length. This prevents R from
        # copying the list every time we add an element
        distractors <- vector(mode = "list", length = 3)
        
        #add distractors to the list.
        # each element of the list should be a set
        # represented as a list.
        for (i in (1:3)) {
          if (i == 1) {
            
            #3^length format
            currentDist <- toString(length(sourceSetsVector), width = NULL)
            currentDist <- paste("\\$", "3^", stringedAnswer, "\\$")
            
            
            #Note the single brackets '[1]' here
            distractors[i] <- currentDist
            
          } else if (i == 2) {
            
            #2 x length 
            currentDist <- toString(length(sourceSetsVector), width = NULL)
            currentDist <- paste("\\$", "2\\times", stringedAnswer, "\\$")
            
            
            #Note the single brackets '[1]' here
            distractors[i] <- currentDist
            
          } else if (i == 3) {
            
            #log format.
            currentDist <-  paste("\\$", "\\log_{2}", correctDisctractor, "\\$")
            
            
            #Note the single brackets '[1]' here
            distractors[i] <- currentDist
            
          }
        }
      }
    }
  } 
  
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  #COpy a
  
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  sourceSets <- str_replace_all(sourceSets, c("list" = ""))
  sourceSets <- str_replace_all(sourceSets, c("\\(" = ""))
  sourceSets <- str_replace_all(sourceSets, c("\\)" = ""))
  sourceSets <- str_replace_all(sourceSets, c("\"" = ""))
  
  
  
  #format the the sourceSets as Question String.
  # "A = {...}"
  sourceSets <- insertSetRStrings(sourceSets)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSets)
  
  #add all items to a list for return
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
  
  return(toSend)
}


#   @param      numSets         The number of sets to consider in the question
#   @param      setSize         The length of the source sets.
#   @param      dType           The desired data type for set elements
#                               (1: Ints, 2: Real, 3: Complex, 
#                               4: Char, 5: String, 6: Mixed)  
#   @param      difficulty     The level of difficulty of the question generated.
#
#   @return     toSend          A json-like object containing the
#                               source sets, correct, and 
#                               distractors (incorrect answers)
#  
cartesianProduct <- function(numSets = 2, setSize = 3, dType = 1, difficulty = 1) {
  
  if (difficulty == 1) {
    questionText <-('Let A and B be two sets. What is \\$A\\times B\\$?')
    
    
    #generate and fill sets
    sourceSets <- getSets(n = numSets, m = setSize, x = dType)
    
    
    #convert each set into a vector to manipulate better
    for(i in (1:2)){
      sourceSets[[i]] <- unlist(sourceSets[[i]])
    }
    
    
    #finds the correct length/cardinality of the cartesian product set.
    setOneLength = length(sourceSets[[1]])
    setTwoLength = length(sourceSets[[2]])
    cartesianProductCardinality = setOneLength * setTwoLength 
    
    
    
    #creates a list to store the pairs when the cartesian product is calculated
    cartesianSet <- list()
    #merge two sets to get a matrix.  
    cartesianSet.df <- merge(sourceSets[[1]], sourceSets[[2]])
    #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
    for(e in (1:cartesianProductCardinality)) {
      cartesianSet[[e]] <- as.vector(cartesianSet.df[e, ])
      
    }
    #makes the cartesesian set into a string
    correct <- toString(cartesianSet, width = NULL) 
    #uses the formatListAsSet function to format the string located in "utils/format.R".
    correct <- formatListAsSet(correct[[1]])
    
    
    
    
    
    
    
    #Create a vector that will hold distractors
    #NOTE: we declare the list with vector() here so that 
    # we can also declare a length. This prevents R from
    # copying the list every time we add an element
    distractors <- vector(mode="list", length = 3)
    
    # add distractors to the list. 
    # each element of the list should be a set
    # represented as a list.
    for(i in (1:3)){
      
      #set generation for Cartesian product. 
      set1 <- (getSets(n = 1, m = (setSize), x = dType))
      set2 <- (getSets(n = 1, m = (setSize), x = dType))
      
      #changes list to vector to better manipulate.
      set1 <- unlist(set1)
      set2 <- unlist(set2)
      
      #gets length for each set. 
      set1Length = length(set1)
      set2Length = length(set2)
      
      #gets length for Cartesian product. 
      cartesianDistractorCardinality = set1Length * set2Length
      
      
      #creates a list to store pairs of Cartesian product. 
      cartesianDistractor <- list()
      
      #merge two sets to get a matrix.  
      cartesianDistractors.df <- merge(set1, set2)
      
      #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
      for(e in (1:cartesianDistractorCardinality)) {
        cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
      }
      
      
      #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
      currentDist <- cartesianDistractor
      currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R
      
      
      #Note the single brackets '[1]' here 
      distractors[i] <- currentDist
    }
    
    
    #formats the "distractor" cartesian product string correctly.
    for(i in (1:3)) {
      temporarySet <- distractors[[i]]
      temporarySet <- str_replace_all(temporarySet, c("list" = ""))
      temporarySet <- str_replace_all(temporarySet, c(":" = ", "))
      temporarySet <- str_replace_all(temporarySet, c("x = " = ""))
      temporarySet <- str_replace_all(temporarySet, c("y = " = ""))
      temporarySet <- str_replace_all(temporarySet, c('\"' = ""))
      distractors[[i]] <- temporarySet
    }
    
    
    
    
    
    
    
    #now we format the sourceSets for output. We waited to do this so we could use
    # the sourceSets for distractor generation.
    
    #Iterate through the sourceSets. format list as Set and insert at the index.
    #COpy a
    
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
    
    
    #format the the sourceSets as Question String.
    # "A = {...}"
    # "B = {...}"
    sourceSets <- insertSetQStrings(sourceSets)
    
    
    #formats the "correct" cartesian product string correctly.
    correct <- formatListAsSet(cartesianSet)
    correct <- str_replace_all(correct,c("list" = ""))
    correct <- str_replace_all(correct, c("x = " = ""))
    correct <- str_replace_all(correct, c("y = " = ""))
    correct <- str_replace_all(correct, c('\"' = ""))
    
    
    
    # now we concatenate the question contents together
    questionContents <- c(questionText, sourceSets)
    
    #format answers and sources into json and return results 
    toSend <- list(content= questionContents, correct= correct, distractors= distractors)
    
    #jsonToSend <- toJSON(toSend)
    
    
    return(toSend)
  }
  
  else if (difficulty == 2) {
    #randomly decides whether the question will A x B or B x A
    randomChoice <- sample((1:2), 1)
    
    
    if (randomChoice == 1 ) {
      
      
      questionText1 <-('Let A and B be two sets. What is \\$A\\times B\\$?')
      
      
      #generate and fill sets
      sourceSets <- getSets(n = numSets, m = sample((2:4),1), x = dType)
      
      #convert each set into a vector to manipulate better
      for(i in (1:2)){
        sourceSets[[i]] <- unlist(sourceSets[[i]])
      }
      
      
      
      #finds the correct length/cardinality of the cartesian product set.
      setOneLength = length(sourceSets[[1]])
      setTwoLength = length(sourceSets[[2]])
      cartesianProductCardinality = setOneLength * setTwoLength 
      
      
      
      #creates a list to store the pairs when the cartesian product is calculated
      cartesianSet <- list()
      #merge two sets to get a matrix.  
      cartesianSet.df <- merge(sourceSets[[1]], sourceSets[[2]])
      #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
      for(e in (1:cartesianProductCardinality)) {
        cartesianSet[[e]] <- as.vector(cartesianSet.df[e, ])
        
      }
      #makes the cartesesian set into a string
      correct <- toString(cartesianSet, width = NULL) 
      #uses the formatListAsSet function to format the string located in "utils/format.R".
      correct <- formatListAsSet(correct[[1]])
      
      
      
      #Create a vector that will hold distractors
      #NOTE: we declare the list with vector() here so that 
      # we can also declare a length. This prevents R from
      # copying the list every time we add an element
      distractors <- vector(mode="list", length = 3)
      
      # three types of distractors are implemented for lvl 2 difficulty: reverse Sets, flipped cartesian product (for example B X A instead of A X B), and random sets. 
      # add distractors to the list. 
      # each element of the list should be a set
      # represented as a list.
      for(i in (1:3)){
        
        #check to see if question is a flipped question
        
        #generate different sets some that are reversed, some normal, some from source but multiplied from in wrong order. 
        
        if (i == 1) {
          
          
          #set generation for Cartesian product.Grabs a completely random set using the getSet function from "utils/set-generation.R'
          randomSet1 <- (getSets(n = 1, m = (setSize), x = dType))
          randomSet2 <- (getSets(n = 1, m = (setSize), x = dType))
          
          #changes list to vector to better manipulate.
          randomSet1 <- unlist(randomSet1)
          randomSet2 <- unlist(randomSet2)
          
          #gets length for each set. 
          set1Length = length(randomSet1)
          set2Length = length(randomSet2)
          
          #gets length for Cartesian product. 
          cartesianDistractorCardinality = set1Length * set2Length
          
          
          #creates a list to store pairs of Cartesian product. 
          cartesianDistractor <- list()
          
          #merge two sets to get a matrix.  
          cartesianDistractors.df <- merge(randomSet1, randomSet2)
          
          #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
          for(e in (1:cartesianDistractorCardinality)) {
            cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
          }
          
          
          #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
          currentDist <- cartesianDistractor
          currentDist <- formatListAsSet(currentDist)  
          
          #Note the single brackets '[1]' here 
          distractors[i] <- currentDist
          
          
        }
        
        else if (i == 2) {
          
          #the reversed sets from the source sets. 
          reverseSet1 <- rev(sourceSets[[1]])
          reverseSet2 <- rev(sourceSets[[2]])
          
          
          #changes list to vector to better manipulate.
          reverseSet1 <- unlist(reverseSet1)
          reverseSet2 <- unlist(reverseSet2)
          
          #gets length for each set. 
          set1Length = length(reverseSet1)
          set2Length = length(reverseSet2)
          
          #gets length for Cartesian product. 
          cartesianDistractorCardinality = set1Length * set2Length
          
          
          #creates a list to store pairs of Cartesian product. 
          cartesianDistractor <- list()
          
          #merge two sets to get a matrix.  
          cartesianDistractors.df <- merge(reverseSet1, reverseSet2)
          
          #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
          for(e in (1:cartesianDistractorCardinality)) {
            cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
          }
          
          
          #assigns the cartesian pairs to currentDist and formats it using formatListAsSet function from "utils/format.R".
          currentDist <- cartesianDistractor
          currentDist <- formatListAsSet(currentDist)  
          
          
          #Note the single brackets '[1]' here 
          distractors[i] <- currentDist
          
        }
        
        else {
          
          #the original sets used in the original question which will be calculated as B X A instead of A X B
          origSet1 <- sourceSets[[1]]
          origSet2 <- sourceSets[[2]]
          
          #append the randomly generated sets with the original sets and reversed sets in correct question 
          
          
          #changes list to vector to better manipulate.
          origSet1 <- unlist(origSet1)
          origSet2 <- unlist(origSet2)
          
          #gets length for each set. 
          set1Length = length(origSet1)
          set2Length = length(origSet2)
          
          #gets length for Cartesian product. 
          cartesianDistractorCardinality = set1Length * set2Length
          
          
          #creates a list to store pairs of Cartesian product. 
          cartesianDistractor <- list()
          
          #merge two sets to get a matrix.  
          cartesianDistractors.df <- merge(origSet2, origSet1)
          
          #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
          for(e in (1:cartesianDistractorCardinality)) {
            cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
          }
          
          
          #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
          currentDist <- cartesianDistractor
          currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R
          
          
          #Note the single brackets '[1]' here 
          distractors[i] <- currentDist
        }
        
        
      }
      
      
      #formats the "distractor" cartesian product string correctly.
      for(i in (1:3)) {
        temporarySet <- distractors[[i]]
        temporarySet <- str_replace_all(temporarySet, c("list" = ""))
        temporarySet <- str_replace_all(temporarySet, c(":" = ", "))
        temporarySet <- str_replace_all(temporarySet, c("x = " = ""))
        temporarySet <- str_replace_all(temporarySet, c("y = " = ""))
        temporarySet <- str_replace_all(temporarySet, c('\"' = ""))
        distractors[[i]] <- temporarySet
      }
      
      
      
      
      
      
      #now we format the sourceSets for output. We waited to do this so we could use
      # the sourceSets for distractor generation.
      
      #Iterate through the sourceSets. format list as Set and insert at the index.
      #COpy a
      
      counter <- 1
      for (s in sourceSets){
        sourceSets[counter] <- formatListAsSet(s)
        counter <- counter + 1
      }
      
      
      #format the the sourceSets as Question String.
      # "A = {...}"
      # "B = {...}"
      sourceSets <- insertSetQStrings(sourceSets)
      
      
      #formats the "correct" cartesian product string correctly.
      correct <- formatListAsSet(cartesianSet)
      correct <- str_replace_all(correct,c("list" = ""))
      correct <- str_replace_all(correct, c("x = " = ""))
      correct <- str_replace_all(correct, c("y = " = ""))
      correct <- str_replace_all(correct, c('\"' = ""))
      
      
      # now we concatenate the question contents together
      questionContents <- c(questionText1, sourceSets)
      
      #format answers and sources into json and return results 
      toSend <- list(content= questionContents, correct= correct, distractors= distractors)
      
      return(toSend)
      
      
      
    } else {
      
      questionText2 <-('Let A and B be two sets. What is \\$B\\times A\\$?')
      
      #generate and fill sets
      sourceSets <- getSets(n = numSets, m = sample((2:4),1), x = dType)
      

      #convert each set into a vector to manipulate better
      for(i in (1:2)){
        sourceSets[[i]] <- unlist(sourceSets[[i]])
      }
      
      
      #finds the correct length/cardinality of the cartesian product set.
      setOneLength = length(sourceSets[[1]])
      setTwoLength = length(sourceSets[[2]])
      cartesianProductCardinality = setOneLength * setTwoLength 
      
      
      
      #creates a list to store the pairs when the cartesian product is calculated
      cartesianSet <- list()
      #merge two sets to get a matrix.  
      cartesianSet.df <- merge(sourceSets[[2]], sourceSets[[1]])
      #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
      for(e in (1:cartesianProductCardinality)) {
        cartesianSet[[e]] <- as.vector(cartesianSet.df[e, ])
        
      }
      #makes the cartesesian set into a string
      correct <- toString(cartesianSet, width = NULL) 
      #uses the formatListAsSet function to format the string located in "utils/format.R".
      correct <- formatListAsSet(correct[[1]])
      
      
      
      #Create a vector that will hold distractors
      #NOTE: we declare the list with vector() here so that 
      # we can also declare a length. This prevents R from
      # copying the list every time we add an element
      distractors <- vector(mode="list", length = 3)
      
      # three types of distractors are implemented for lvl 2 difficulty: reverse Sets, flipped cartesian product (for example B X A instead of A X B), and random sets. 
      # add distractors to the list. 
      # each element of the list should be a set
      # represented as a list.
      for(i in (1:3)){
        
        #check to see if question is a flipped question
        
        #generate different sets some that are reversed, some normal, some from source but multiplied from in wrong order. 
        
        if (i == 1) {
          
          
          #set generation for Cartesian product.Grabs a completely random set using the getSet function from "utils/set-generation.R'
          randomSet1 <- (getSets(n = 1, m = (setSize), x = dType))
          randomSet2 <- (getSets(n = 1, m = (setSize), x = dType))
          
          #changes list to vector to better manipulate.
          randomSet1 <- unlist(randomSet1)
          randomSet2 <- unlist(randomSet2)
          
          #gets length for each set. 
          set1Length = length(randomSet1)
          set2Length = length(randomSet2)
          
          #gets length for Cartesian product. 
          cartesianDistractorCardinality = set1Length * set2Length
          
          
          #creates a list to store pairs of Cartesian product. 
          cartesianDistractor <- list()
          
          #merge two sets to get a matrix.  
          cartesianDistractors.df <- merge(randomSet1, randomSet2)
          
          #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
          for(e in (1:cartesianDistractorCardinality)) {
            cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
          }
          
          
          #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
          currentDist <- cartesianDistractor
          currentDist <- formatListAsSet(currentDist)  
          
          #Note the single brackets '[1]' here 
          distractors[i] <- currentDist
          
          
        }
        
        else if (i == 2) {
          
          #the reversed sets from the source sets. 
          reverseSet1 <- rev(sourceSets[[1]])
          reverseSet2 <- rev(sourceSets[[2]])
          
          
          #changes list to vector to better manipulate.
          reverseSet1 <- unlist(reverseSet1)
          reverseSet2 <- unlist(reverseSet2)
          
          #gets length for each set. 
          set1Length = length(reverseSet1)
          set2Length = length(reverseSet2)
          
          #gets length for Cartesian product. 
          cartesianDistractorCardinality = set1Length * set2Length
          
          
          #creates a list to store pairs of Cartesian product. 
          cartesianDistractor <- list()
          
          #merge two sets to get a matrix.  
          cartesianDistractors.df <- merge(reverseSet2, reverseSet1)
          
          #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
          for(e in (1:cartesianDistractorCardinality)) {
            cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
          }
          
          
          #assigns the cartesian pairs to currentDist and formats it using formatListAsSet function from "utils/format.R".
          currentDist <- cartesianDistractor
          currentDist <- formatListAsSet(currentDist)  
          
          
          #Note the single brackets '[1]' here 
          distractors[i] <- currentDist
          
        }
        
        else {
          
          #the original sets used in the original question which will be calculated as A X B instead of B X A
          origSet1 <- sourceSets[[1]]
          origSet2 <- sourceSets[[2]]
          
          #append the randomly generated sets with the original sets and reversed sets in correct question 
          
          
          #changes list to vector to better manipulate.
          origSet1 <- unlist(origSet1)
          origSet2 <- unlist(origSet2)
          
          #gets length for each set. 
          set1Length = length(origSet1)
          set2Length = length(origSet2)
          
          #gets length for Cartesian product. 
          cartesianDistractorCardinality = set1Length * set2Length
          
          
          #creates a list to store pairs of Cartesian product. 
          cartesianDistractor <- list()
          
          #merge two sets to get a matrix.  
          cartesianDistractors.df <- merge(origSet1, origSet2)
          
          #converts the matrix back to a vector (which is populated with the "cartesian" pairs). 
          for(e in (1:cartesianDistractorCardinality)) {
            cartesianDistractor[[e]] <- as.vector(cartesianDistractors.df[e, ])
          }
          
          
          #assigns the cartesian pairs list to currentDist and formats it using formatListAsSet function from "utils/format.R".
          currentDist <- cartesianDistractor
          currentDist <- formatListAsSet(currentDist)  
          
          
          #Note the single brackets '[1]' here 
          distractors[i] <- currentDist
        }
        
        
      }
      
      
      #formats the "distractor" cartesian product string correctly.
      for(i in (1:3)) {
        temporarySet <- distractors[[i]]
        temporarySet <- str_replace_all(temporarySet, c("list" = ""))
        temporarySet <- str_replace_all(temporarySet, c(":" = ", "))
        temporarySet <- str_replace_all(temporarySet, c("x = " = ""))
        temporarySet <- str_replace_all(temporarySet, c("y = " = ""))
        temporarySet <- str_replace_all(temporarySet, c('\"' = ""))
        distractors[[i]] <- temporarySet
      }
      
      
      
      
      
      
      #now we format the sourceSets for output. We waited to do this so we could use
      # the sourceSets for distractor generation.
      
      #Iterate through the sourceSets. format list as Set and insert at the index.
      #COpy a
      
      counter <- 1
      for (s in sourceSets){
        sourceSets[counter] <- formatListAsSet(s)
        counter <- counter + 1
      }
      
      
      #format the the sourceSets as Question String.
      # "A = {...}"
      # "B = {...}"
      sourceSets <- insertSetQStrings(sourceSets)
      
      
      #formats the "correct" cartesian product string correctly.
      correct <- formatListAsSet(cartesianSet)
      correct <- str_replace_all(correct,c("list" = ""))
      correct <- str_replace_all(correct, c("x = " = ""))
      correct <- str_replace_all(correct, c("y = " = ""))
      correct <- str_replace_all(correct, c('\"' = ""))
      
      
      # now we concatenate the question contents together
      questionContents <- c(questionText2, sourceSets)
      
      #format answers and sources into json and return results 
      toSend <- list(content= questionContents, correct= correct, distractors= distractors)
      
      return(toSend)
      
      
    }
  } else if(difficulty == 3) {
    
    #generate and fill sets.
    sourceSets <- getSets(n = 3, m = 3, x = 1)
    #assign each set to a variable. 
    a <- unlist(sourceSets[[1]])
    b <- unlist(sourceSets[[2]])
    c <- unlist(sourceSets[[3]])
    #randomly generates question asked
    randomChoice <- sample(1:5, 1)
    
    #functions that get cartesian product of 3 sets 
    cartesian3sets <- function(h, i, j) {
      hxixj <- list()
      k <- 1
      for (t in h) {
        for (y in i) {
          for( u in j) {
            hxixj[[k]] <- c(t,y,u)
            k <- k + 1
          }
        }
      }
      return(hxixj)
    }
    
    #functions that get cartesian product of 2 sets
    cartesian2sets <- function(h, i) {
      hxi <- list()
      k <- 1
      for (t in h) {
        for (y in i) {
          hxi[[k]] <- c(t,y)
          k <- k + 1
        }
      }
      return(hxi)
    }
    
    if(randomChoice == 1){
      questionType <- sample(1:2, 1)
      
      if(questionType == 1) {
        questionText <-('Let A, B, and C be three sets. What is \\$A\\times (B\\cap C)\\$?')
      } else {
        questionText <-('Let A, B, and C be three sets. What is \\$(A\\times B) \\cap (A\\times C)\\$?')
      }
      
      correct <- cartesian2sets(a, intersect(b,c))
      correct <-formatListAsSet(correct)
      
      correct <- str_replace_all(correct, c("c" = ""))
    } else if(randomChoice == 2) {
      
      questionType <- sample(1:2, 1)
      
      if(questionType == 1) {
        questionText <-('Let A, B, and C be three sets. What is \\$A\\times (B\\cup C)\\$?')
      } else {
        questionText <-('Let A, B, and C be three sets. What is \\$(A\\times B) \\cup (A\\times C)\\$?')
      }
      
      correct <- cartesian2sets(a, union(b,c))
      correct <-formatListAsSet(correct)
      
      correct <- str_replace_all(correct, c("c" = ""))
      
    } else if(randomChoice == 3) {
      
      questionType <- sample(1:2, 1)
      
      if(questionType == 1) {
        questionText <-('Let A, B, and C be three sets. What is \\$(A\\cap B) \\times C\\$?')
      } else {
        questionText <-('Let A, B, and C be three sets. What is \\$(A\\times C) \\cap (B\\times C)\\$?')
      }
      
      correct <- cartesian2sets(intersect(a,b), c)
      correct <-formatListAsSet(correct)
      
      correct <- str_replace_all(correct, c("c" = ""))
      
    } else if(randomChoice == 4) {
      
      questionType <- sample(1:2, 1)
      
      if(questionType == 1) {
        questionText <-('Let A, B, and C be three sets. What is \\$(A\\cup B) \\times C\\$?')
      } else {
        questionText <-('Let A, B, and C be three sets. What is \\$(A\\times C) \\cup (B\\times C)\\$?')
      }
      
      
      
      correct <- cartesian2sets(union(a,b), c)
      correct <-formatListAsSet(correct)
      
      
      correct <- str_replace_all(correct, c("c" = ""))
      
    } else if(randomChoice == 5) {
      
      questionText <-('Let A, B, and C be three sets. What is \\$A\\times B \\times C \\$?')
      
      correct <- cartesian3sets(a, b, c)
      
      temporarySet <- correct
      temporarySet <- str_replace_all(temporarySet, c("list\\(" = "\\(\\"))
      temporarySet <- str_replace_all(temporarySet, c("c" = ""))
      temporarySet <- str_replace_all(temporarySet, c("\\)," = "],"))
      temporarySet <- str_replace_all(temporarySet, c("\\)" = "]"))
      temporarySet <- str_replace_all(temporarySet, c("\\[" = "\\("))
      temporarySet <- str_replace_all(temporarySet, c("]," = "\\),"))
      temporarySet <- str_replace_all(temporarySet, c("]" = "\\)"))
      correct <- temporarySet
      
      correct <-formatListAsSet(correct)
    }
    
    
    #Create a vector that will hold distractors
    #NOTE: we declare the list with vector() here so that 
    # we can also declare a length. This prevents R from
    # copying the list every time we add an element
    distractors <- vector(mode="list", length = 3)
    
    
    for(i in (1:3)){
      
      #generate and fill sets.
      distractorSet <- getSets(n = 3, m = 2, x = 1)
      #assign each set to a variable. 
      x <- unlist(distractorSet[[1]])
      y <- unlist(distractorSet[[2]])
      z <- unlist(distractorSet[[3]])
      
      
      currentDist <- cartesian3sets(x, y, z)
      temporarySet <- currentDist
      temporarySet <- str_replace_all(temporarySet, c("list\\(" = "\\(\\"))
      temporarySet <- str_replace_all(temporarySet, c("c" = ""))
      temporarySet <- str_replace_all(temporarySet, c("\\)," = "],"))
      temporarySet <- str_replace_all(temporarySet, c("\\)" = "]"))
      temporarySet <- str_replace_all(temporarySet, c("\\[" = "\\("))
      temporarySet <- str_replace_all(temporarySet, c("]," = "\\),"))
      temporarySet <- str_replace_all(temporarySet, c("]" = "\\)"))
      currentDist <- temporarySet
      
      
      currentDist <- formatListAsSet(currentDist)  #The [[1]] is important here as it removes a layer of abstraction imposed by R
      
  
      #Note the single brackets '[1]' here 
      distractors[i] <- currentDist
      
    }
    
    #now we format the sourceSets for output. We waited to do this so we could use
    # the sourceSets for distractor generation.
    
    #Iterate through the sourceSets. format list as Set and insert at the index.
    #COpy a
    
    counter <- 1
    for (s in sourceSets){
      sourceSets[counter] <- formatListAsSet(s)
      counter <- counter + 1
    }
    
    
    #format the the sourceSets as Question String.
    # "A = {...}"
    # "B = {...}"
    # "C = {...}"
    sourceSets <- insertSet3Strings(sourceSets)
    
    
    
    # now we concatenate the question contents together
    questionContents <- c(questionText, sourceSets)
    
    #format answers and sources into json and return results 
    toSend <- list(content= questionContents, correct= correct, distractors= distractors)
    
    #jsonToSend <- toJSON(toSend)
    
    
    return(toSend)
  }
}
