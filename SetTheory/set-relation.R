# Author:         Trevor Strobel
# File:           set-Relation.R

library(set)


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
  
  #add distractors to the list. 
  # each element of the list should be a set
  # represented as a list.
  
  for(i in (1:3)){
    #generate a set
    currentDist <- NULL
    if(difficulty > 1){ #difficulty higher than 1 scrambles lists in output.
      currentDist <- sample(correct, length(correct), replace = FALSE)
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

getAsymDiffMC <- function(n=2, m=5) {
  
  n <-2   #currently, the api only supports 2 sets. 
  
  sourceSets <- list()    #creating an empty list. Elements will be sets 
  
  #for each in a given number of sets, fill the set with ints (1:9)
  for(e in (1:n)) {
    sourceSets[[e]] <- sample(1:9, m, replace = F)
  }
  allElements <- vector()
  
  for(e in sourceSets) {
    #print(e)
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

getSetCompliment <- function(n=1, m = 5) {

  sourceSets <- list()    #creating an empty list. Elements will be sets 
  
  #for each in a given number of sets, fill the set with ints (1:9)
  for(e in (1:n)) {
    sourceSets[[e]] <- sample(1:9, m, replace = F)
  }
  allElements <- vector()
  
  for(e in sourceSets) {
    #print(e)
    allElements <- c(allElements, e)
  }
  
}


x <- getSetUnionMC()
