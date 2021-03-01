# Author:         Trevor Strobel
# File:           set-Relation.R

library(plumber)

#The purpose of this file is provide an API from which
# a web browser may request a problem to solve regarding
# set operations. 



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
  numEntries <- 2*5 
  wrongs <- list() #creates an empty list of wrong answers
  iWrongs <- 1     #index of wrong answer list.
  
  #creates a single vector. Ints ranging 1 to 20. 20 ints in the vector.
  intVec <- sample(1:20, numEntries, replace=T)
  
  sets <- list()    #creating an empty list. Elements will be sets 
  
  
  #for each in a given number of sets
  for(e in (1:n)) {
    sets[[e]] <- sample(1:20, m, replace = T)
  }

  #only going to work for 2 sets so far. 
  answer <- union(sets[[1]], sets[[2]])
  answer <- answer[order(answer)]
  #print(answer)
  
  
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
    wrongs[[iWrongs]] <- dupeSets
    iWrongs <- iWrongs + 1
    
  }
  
  for(e in (iWrongs:3)) {
    wrongs[[iWrongs]] <- (sample(1:20, (5 *2), replace = T))
    iWrongs <- iWrongs + 1
  }
    
  for (e in wrongs) {
    print("check")
    print(e)
  }
  
  return(answer)
  
}






getSetUnion(2, 5)