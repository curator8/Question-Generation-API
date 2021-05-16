#Author:                Trevor Strobel
#Date:                  5/13/2021

# This document provides multiple element generation functions for use in the 
# ISPeL Question Generation API. The elements generated are for use in any
# questions that require a set, array, etc. filled with data.




# Integer generation
# param   size      the number of requested integers
# param   min       minimum integer value
# param   max       maximum integer value
# param   repl      should the sample generation use replacement?  
# return            vector of integers
getInt <- function(size = 1, min = 0, max = 10000, repl= FALSE ){
  
  if((max - min) < size){
    print("The size of the requested list is larger than the pool of available integers. Forcing replacement...")
    repl = TRUE
  }
    return(sample(min:max, size, replace = repl))
}

#Real Number Generation
# param    size      the number of requested Real numbers
# param   min       minimum real number value
# param   max       maximum real number value
# param   dec       maximum number of decimal places (max = 6)
# return            list of real numbers of length
getReal <- function(size = 1, min = 0, max = 10000, dec = 6){
  reals <- vector(mode = "list")
  x <- runif(size, min, max)
  for(num in x){
    points <- sample(1:dec, 1)
    reals  <- c(reals, round(num, digits = points))
  }
  return(reals)
}


#Complex Number Generation



#dev testing code.
x <- getInt(4)
print(x)

x <- getReal(5, 0, 10, 4)
print(x[[5]] + x[[4]])