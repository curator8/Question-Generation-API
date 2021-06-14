# Author:         Trevor Strobel
# File:           set-Relation.R


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

getSetIntersectMC <- function(numSets=2, setSize=5, dType = 1, difficulty = 1) {
  
  #define the text of the question
  questionText <-('Let A and B be two sets. What is \\$A\\cap B\\$?')
  
  #generate and fill sets
  sourceSets <- getSets(n = numSets, m = setSize, x = dType)
  
  #creating the correct answer
  correct <- intersect(sourceSets[[1]], sourceSets[[2]])
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
getSetComplementMC <- function(numSets = 2, setSize = 9, dType = 1) {
  
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
  correct <- formatListAsSet(correct)
  
  
  
  distractors <- vector(mode="list", length = 3)
  
  #distractor 1 Is similar to the correct answer, but with one different value
  d1 <- replace(d1, length(d1) - 2, getValue(x = dType, min = 21, max = 30)) 
  
  #distractor 2 is also similar to the correct answer, but with one replaced value
  d2 <- replace(d2, length(d2), getValue(x = dType, min = 21, max = 30))
  
  #distractor 3 is the original set which is not the complement and is wrong
  d3 <- sourceSets[[1]]
  
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
getSetCardinalityMC <- function(numSets = 1, setSize = sample(1:9, 1, replace = FALSE), dType = 1) {
  #define the text of the question
  questionText <-('Let A be a set. What is the cardinality of set A?')
  #generate and fill sets
  sourceSet <- getSets(n = numSets, m = setSize, x = dType)
  
  
  #creating the correct answer
  #NOTE: this will change based on the desired algorithm,
  #       but the answer should be stored as a list
  # 
  # Here, the union is used as an example
  correct <- lengths(sourceSet) 
  
  distractors <- vector(mode="list", length = 3)
  
  distractors[[1]] <- lengths(sourceSet) - 1
  
  distractors[[2]] <- lengths(sourceSet) + 1
  
  distractors[[3]] <- lengths(sourceSet) + 5
  
  
  #Iterate through the sourceSets. format list as Set and insert at the index.
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
  


