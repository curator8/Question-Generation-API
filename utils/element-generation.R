#Author:                Trevor Strobel
#Date:                  5/13/2021

# This document provides multiple element generation functions for use in the 
# ISPeL Question Generation API. The elements generated are for use in any
# questions that require a set, array, etc. filled with data.

#For uniformity and compatibility, each method returns a list of
# the desired data type(s).

library('stringi')


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
# param   type      int(1:2) The data type of the coefficient 
#                   (1: int, 2: real)
#return             List of complex numbers
getComplex <- function(size = 1, min = 0, max = 10000, type = 1){
  complexNums <- list()
  
  for(i in(1:size)){ #integer type coefficients
    z <- 0
    if(type==1){
      z <- complex(real = getInt(1), imaginary = getInt(1))
    }
    else if(type ==2){ #real type coefficients
      z <- complex(real = getReal(1), imaginary = getReal(1))
    }
  }
    
    complexNums <- c(z, complexNums)

  return(complexNums)
}
  

# Character Generation
# param   size      Number of character 'strings' to return
# param   length    Length of the character string(s)
#
# return            List of character strings
getCharString <- function(size = 1, length = 1){
  chars <- list()
  
  for(i in (1:size)){
    x <- stri_rand_strings(1,length, pattern = "[A-Za-z]")
    chars <- c(x, chars)
  }
  return(chars)
}


#String Generation
# param size      Number of Strings to return
# param cat       The category of strings (1:6)
#                 (1: all, 2: Country names, 3: City Names
#                 4: Male Names, 5: Female Names, 6: Names)
getStrings <- function(size = 1, cat = 6){
  strings <- list()
  
  data <- readLines("utils/StringData/names.csv")
  for(x in (1:size)){
    i <- sample.int(length(data), 1)
    d <- data[i]
    strings <- c(d, strings)
    
  }
  return(strings)
}

x <- getStrings(3)
print(x)




