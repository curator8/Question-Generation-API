library(httr)
library(jsonlite)
library(venn)
library(plumber)



# getBlankDiag(n)
# returns a set diagram with no shading. This function should
# always be called as it draws the baselines for each set in the
# diagram. 
# @param n      number of sets in the diagram

getBlankDiag <- function(n) {
  return(venn(n))
}


# getSetRelation(n)
# returns a relation on n sets in the form "AB~C" or "BC"
getSetRelation <- function(n) {
  #creates a vector of size n containing random numbers between 0 and 3
  #     entries starting with "0" are flagged as "active"
  #     entries starting with "1" are flagged as "intersect active"
  #     entries starting with "2" are flagged as "inactive"
  # For instance, consider 3 sets. Their entries in the random Vector are
  # [0.223, 2.314, 1.0001].  The resulting relation would be, "A~C"
  
  randVect = (runif(n, min=0, max = 2.9))

  #TODO: remove. For debugging only.
  #print(randVect)
  
  relation = ""
  
  count = 1  # indicates which set we're considering. 1 = A, 2 = B, etc.
  for (val in randVect) {
    if(val >= 0 && val < 1) {
      relation <- paste (relation, "0", sep = "")
      count = count + 1
    } else if(val >= 1 && val < 2) {
      relation <- paste (relation, "1", sep = "")
      count = count + 1
    } else {
      relation <- paste (relation, "-", sep = "")
      count = count + 1
    }
    
  }
  print(relation)
  return(relation)
}

 #drawRelation(rel) shades the area defined by the set relation "rel"

drawRelation <- function(rel) {
  
  area <- getZones(rel)
  #area <- getZones("~A B ", snames = "A, B, C")
  for(val in area[]) {
    polygon(val, col = "lightblue")
  }
}







getBlankDiag(3)
drawRelation(getSetRelation(3))





