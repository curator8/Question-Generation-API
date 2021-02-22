library(venn)
library(base64enc)
library(jsonlite)
library(plumber)


# getSetRelation(n)
# returns a relation on n sets in the form "AB~C" or "BC"
getSetRelation <- function(n) {
  #creates a vector of size n containing random numbers between 0 and 3
  #     entries starting with "0" are flagged as "active"
  #     entries starting with "1" are flagged as "intersect active"
  #     entries starting with "2" are flagged as "inactive"
  # For instance, consider 3 sets. Their entries in the random Vector are
  # [0.223, 2.314, 1.0001].  The resulting relation would be, "A~C"
  
  randVect = (runif(n, min=0, max = 3.9))
  
  #TODO: remove. For debugging only.
  print(randVect)
  
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
  relationStr = ""
  for(val in relationVec) {
    relationStr <- paste0(relationStr, val)
  }
  print("Pre-cleaning")
  print(relationStr)
  relationStr <- cleanRelationStr(relationStr)
  print("post cleaning")
  print(relationStr)
  return(relationStr)
}




#cleanRelationStr takes a string containing a set relation and removes
# any leading "*" or "+". 
# NOTE: a leading "*" or "+" is result of the randomizer in getSetRelation
cleanRelationStr <- function(rel) {
  if(substr(rel,1,1) == "*" || substr(rel,1,1) == "+") {
    print("ping!")
    
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

middleWare <- function(solution) {
  
    
}

#getSetProblem() returns a json object containing a base64 encoded png
#of a set relation and a string with the correct set relation definition.

getSetProblem <- function() {
  solution <-getSetRelation(2)

  #for conversion to base64, the png of the plot must be written to disk. (as far as I can tell)
  png(file="set_diag_tmp.png")
  venn(solution, snames = "A, B",sncs = 4)  #this is the line that actually draws the plot in the buffer. 
  dev.off()  #saves the above plot to disk

  encodedDiag <- encodeDiagram("set_diag_tmp.png")
  transData <- data.frame(solution, encodedDiag)
  json <- toJSON(transData, pretty=TRUE)
  
  
  print(json) #TODO: remove. for debugging only. 
  
  return(json)
  
  
}



getSetProblem()




