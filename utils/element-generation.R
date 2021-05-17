#Author:                Trevor Strobel
#Date:                  5/13/2021

# This document provides multiple element generation functions for use in the 
# ISPeL Question Generation API. The elements generated are for use in any
# questions that require a set, array, etc. filled with data.

#For uniformity and compatibility, each method returns a list of
# the desired data type(s).




# Integer generation
# param   size      the number of requested integers
# param   min       minimum integer value
# param   max       maximum integer value
# param   repl      should the sample generation use replacement?  
# return            list of integers
getInt <- function(size = 1, min = 0, max = 10000, repl= FALSE ){
  ints <- list()
  if((max - min) < size){
    print("The size of the requested list is larger than the pool of available integers. Forcing replacement...")
    repl = TRUE
  }
  
    for(e in (1:size)){
      ints[[e]] <- sample(min:max, 1, replace = repl)
    }
  
    return(ints)
}

#Real Number Generation
# param    size      the number of requested Real numbers
# param   min       minimum real number value
# param   max       maximum real number value
# param   dec       maximum number of decimal places (max = 6)
# return            list of real numbers
getReal <- function(size = 1, min = 0, max = 10000, dec = 6){
  reals <- list()
  x <- runif(size, min, max)
  for(num in x){
    points <- sample(1:dec, 1)
    reals  <- c(reals, round(num, digits = points))
  }
  return(reals)
}


#Complex Number Generation
# param   size      The number of requested complex nubmers
# param   min       The minimum real or complex coefficient
# param   max       The maximum real or complex coefficient
# param   type      int(1:3) The data type of the coefficient 
#                   (1: int, 2: real, 3: mixed)
#return             List of complex numbers



#dev testing code.
print("getint(4)")
x <- getInt(4)
print(x)

print("getReal(5,0,10,4)")
x <- getReal(5, 0, 10, 4)
for (e in x){
  print(e)
}

#complex numbers
print("Complex nubmers:")
z <- complex(real = getInt(1), imaginary = getInt(1))
print(z)