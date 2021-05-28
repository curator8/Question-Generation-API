library(venn)
library(base64enc)
library(jsonlite)
library(plumber)


# getSetRelation(n)
# returns a relation on n sets in the form "AB~C" or "BC"
getSetRelation <- function(n=2) {
  #creates a vector of size n containing random numbers between 0 and 3
  #     entries starting with "0" are flagged as "active"
  #     entries starting with "1" are flagged as "intersect active"
  #     entries starting with "2" are flagged as "inactive"
  # For instance, consider 3 sets. Their entries in the random Vector are
  # [0.223, 2.314, 1.0001].  The resulting relation would be, "A~C"
  
  randVect = (runif(n, min=0, max = 3.9))
  
  #an empty vector. will hold set names and operators
  relationVec <- c()
  
  count = 1 #Used to keep track of what set is being considered. 1 = A, 2 = B, etc.
  for (val in randVect) {
    if(val >= 0 && val < 1) {
  
          
      val <- paste0("+", LETTERS[count])
      relationVec <- c(relationVec, val)
      count = count + 1
      
    } else if(val >= 1 && val < 2) {
      
      val <- paste0("*", LETTERS[count])
      relationVec <- c(relationVec, val)
      count = count + 1
      
      
    } else if (val >=2 && val < 3) {
      
      val <- paste0("~", LETTERS[count])
      relationVec <- c(relationVec, val)
      count = count + 1
      
      
      } else { #this option allows for a set to not be ommitted from the set relation. 
        #TODO: currently, randomness allows for all values to be skipped. Fix that issue. 
      
      count = count + 1
    }
  }
  
  #occasionally all values would be over 3, resulting in an empty relationship. 
  #this just assigns "A" to the vector if it's empty.
  if(is.null(relationVec)){
    relationVec[1] <- "A"
  }
  
  
  relationStr = ""
  for(val in relationVec) {
    relationStr <- paste0(relationStr, val)
  }
 
  #cleaning the string up to prevent nonsense relations
  relationStr <- cleanRelationStr(relationStr)
  
  return(relationStr)
}




#cleanRelationStr takes a string containing a set relation and removes
# any leading "*" or "+". 
# NOTE: a leading "*" or "+" is result of the randomizer in getSetRelation
cleanRelationStr <- function(rel) {
  if(substr(rel,1,1) == "*" || substr(rel,1,1) == "+") {
    return(sub('.', '', rel))
  
    } else
    {
      return(rel)
    }
}


#encodeDiagram(diag) converts a png of a set diagram to base64 and preps it for display in web browsers.
# @params diag    path to the diagram png
encodeDiagram <- function(diag) {
  encVec <-base64encode(diag)
  encVec <- paste0("data:image/png;base64,", encVec)
  
  return(encVec)
}

#getSetProblem() returns a json object containing a base64 encoded png
#of a set relation and a string with the correct set relation definition.
#* @param n number of sets in the diagram
#* @response serializer pngssin
#* @get  /getSetProblem
getSetProblem <- function(n=2) {
  solution <-getSetRelation(2)
  print("called function")
  #for conversion to base64, the png of the plot must be written to disk. (as far as I can tell)
  png(file="set_diag_tmp.png")
  venn(solution, snames = "A, B",sncs = 4)  #this is the line that actually draws the plot in the buffer. 
  dev.off()  #saves the above plot to disk

  encodedDiag <- encodeDiagram("set_diag_tmp.png")
  pngData <- data.frame(solution, encodedDiag)
  
  
  #distractor Generation
  wrongs <- list()
  wrongs[[1]] <- getSetRelation()
  wrongs[[2]] <- getSetRelation()
  wrongs[[3]] <- getSetRelation()

  #replacing any generated distractors that match the correct solution
  i <- 1
  for(e in wrongs){
    
    while(e == solution){
      wrongs[[i]] <- getSetRelation()
      e <- wrongs[[i]]
    }
    i <- i + 1 #incrementing counter for indexing list
  }
  
  
  #replacing any generated distractors that match another distractor
  
  #DEV-NOTE: 3/21/21 TJS. This may be the most lazy code I've ever written, but from what I read about Loops in R, 
  # it may actually be more memory efficient than for loops.
  while(wrongs[[1]] ==wrongs[[2]] || wrongs[[2]] == wrongs[[3]] || wrongs[[1]]==wrongs[[3]]){
    if(wrongs[[1]] == wrongs[[2]]){
      while(wrongs[[1]] == wrongs[[2]]){
        wrongs[[2]] <= getSetRelation()
      }
    }
    
    if(wrongs[[2]] == wrongs[[3]]){
      while(wrongs[[2]] == wrongs[[3]]){
        wrongs[[3]] <= getSetRelation()
      }
    }
    
    if(wrongs[[1]] == wrongs[[3]]){
      while(wrongs[[1]] == wrongs[[3]]){
        wrongs[[3]] <= getSetRelation()
      }
    }
  }
  
  toSend <- list(source= pngData, answer= solution, wrongs= wrongs)
  
  jsonToSend <- toJSON(toSend, pretty = TRUE)
  
  
  
  return(jsonToSend)
  
}



