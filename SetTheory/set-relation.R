# Author:         Trevor Strobel, Joel Montano, Christopher A. Wright
# File:           set-Relation.R


library(set)
library(nsprcomp)
library(sets)


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
  #define the text of the question
  questionText <-('Let A and B be two sets. What is \\$A\\cup B\\$?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- union(sourceSets[[1]], sourceSets[[2]])
  if(difficulty > 1){
    correct <- sample(correct, length(correct), replace = FALSE)
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
  
  questionStr <- "Let A and B be two sets. What is A-B?"
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- not(sourceSets[[1]], sourceSets[[2]])
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
    if(difficulty > 1){ #difficulty higher than 1 scrambles lists in output.
      currentDist <- sample(correct, replace = FALSE)
    }
    currentDist <- correct
    
    if(i == 1){ #empty set or set intersect
      if(currentDist == "\\$\\emptyset\\$"){
        currentDist <- intersect(sourceSets[[1]], sourceSets[[2]])
        currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
        
      } else {
        currentDist <- "\\$\\emptyset\\$"
      }
    }
    else if(i ==2){ #add an element
      currentDist <- not(sourceSets[[1]], sourceSets[[2]])
      currentDist <- list(c(currentDist, getValue(x=dType)))
      currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
      
    }
    else if(i == 3){ #remove an element
      currentDist <- not(sourceSets[[1]], sourceSets[[2]])
      if(length(currentDist > 1)){
        currentDist <- list(currentDist[-1])
      }else { #add an element
        currentDist <- list(c(currentDist, getValue(x=dType)))
      }
      currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    }
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  
  #now we format the sourceSets for output. We waited to do this so we could use
  # the sourceSets for distractor generation.
  
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
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  sourceSets[[1]] <- sourceSets[[2]]
  #scramble Universal set
  sourceSets[[2]] <- sample(sourceSets[[2]], length(sourceSets[[2]]), replace  = FALSE)
  length(sourceSets[[1]]) <- 5
  
  correct <- not(sourceSets[[2]], sourceSets[[1]])
  d1 <- correct
  d2 <- correct

  #shuffles when diff over 1
  if(difficulty > 1){
    correct <- sample(correct, length(correct), replace = FALSE)
  }
  correct <- formatListAsSet(correct)
  
  
  
  distractors <- vector(mode="list", length = 3)
  
  #distractor 1 Is similar to the correct answer, but with one different value
  d1 <- replace(d1, length(d1) - 2, getValue(x = dType, min = 21, max = 30)) 
  if(difficulty > 1){
    d1 <- sample(d1, replace= FALSE)
  }
  #distractor 2 is also similar to the correct answer, but with one replaced value
  d2 <- replace(d2, length(d2), getValue(x = dType, min = 21, max = 30))
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
  distractors[[3]] <- formatListAsSet(d3)
  
  
  
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

getSetEqualityMC <- function(numSets = 2, setSize = 5, dType = 1) {
  questionText <- "Let A and B be two sets. Are A and B equal?"
  #Hard Coded this as the function only works with two sets at the moment.
  numSets <- 2
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #sets 50/50 probability of generated sets being equal or not.
  probability <- sample(1:2, 1, replace = FALSE)
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
  
  #creating the correct answer based on length of SourceSet
  correct <- lengths(sourceSet) 
  
  #Creating distractors based on correct answer.
  distractors <- vector(mode="list", length = 3)
  probability <- sample(1:2, 1, replace = FALSE)
  
  for(i in 1:3) {
    if (probability == 1) {
      distractors[[i]] <- lengths(sourceSet) - sample(1:2, 1, replace = FALSE)
    }
    if (probability == 2) {
      distractors[[i]] <- lengths(sourceSet) + sample(1:2, 1, replace = FALSE)
    }
  }
  
  #Iterate through the sourceSet. format list as Set and insert at the index.
  counter <- 1
  for (s in sourceSet){
    sourceSet[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }
  
  #format the the sourceSet as Question Strings
  # "A = {...}"
  sourceSets <- insertSetRStrings(sourceSet)
  
  # now we concatenate the question contents together
  questionContents <- c(questionText, sourceSet)
  
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
  
getSymmDiffMC <- function(numSets = 2, setSize = 5, dType = 1){
  
  #define the text of the question
  questionText <-('Let A and B be two unique sets. What is the symmetric difference of A and B?')
  
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

  #Iterate through the sourceSets. format list as Set and insert at the index.
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
  }

  #format the the sourceSet as Question Strings
  # "A = {...}"
  sourceSets <- insertSetQStrings(sourceSets)

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

getSetPartitionsMC <- function(numSets = 1, setSize = 5, dType = 1) {
  
  #define the text of the question
  questionText <-('Let A be a set. Which answer represents an incorrect set partition of set A?')
  
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
  
  # Replaces one member of the first partition with a random member, thus making
  # the partitioning incorrect and generating the correct answer choice.
  initial <- replace(initial, length(initial) - sample(0:2, 1, replace = FALSE),
  getValue(x = dType, min = 1, max = 20))
  
  # Formats each inner partition as a set and concatenates each set
  # within the larger list. Then formats the larger list as a set.
  initial <- formatPartitionAsSet(initial)
  secondSet <- formatPartitionAsSet(secondSet)
  correct <- c(correct, initial)
  correct <- c(correct, secondSet)
  correct <- formatListAsSet(correct)
  
  distractors <- vector(mode="list", length = 3)
  
  
  for(i in (1:3)){
    #generate and partition distractor sets
    currentDist <- (getSets(n = 1, m = 5, x = dType))
    firstSet <- sample(sourceSets[[1]], length(sourceSets[[1]]), replace  = FALSE)
    length(firstSet) <- sample(2:4, 1, replace = FALSE)
    secondSet <- not(sourceSets[[1]], firstSet)
    firstSet <- formatPartitionAsSet(firstSet)
    secondSet <- formatPartitionAsSet(secondSet)
    wrong <- list()
    # and concatenating both sets inside larger empty list.
    # Wrong variable is created to deal with weird out of bounds issue in R.
    wrong <- c(wrong, firstSet)
    wrong <- c(wrong, secondSet)
    currentDist[[1]] <- wrong
    currentDist <- formatListAsSet(currentDist[[1]])  #The [[1]] is important here as it removes a layer of abstraction imposed by R
    
    #Note the single brackets '[1]' here 
    distractors[i] <- currentDist
  }
  
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
  counter <- 1
  for (s in sourceSets){
    sourceSets[counter] <- formatListAsSet(s)
    counter <- counter + 1
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
            currentDist <- paste("\\$", "2\times", stringedAnswer, "\\$")
            
            
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
  }
}
