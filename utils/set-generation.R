#Author:                Trevor Strobel
#Date:                  5/13/2021

# This document provides multiple element generation functions for use in the 
# ISPeL Question Generation API. The elements generated are for use in any
# questions that require a set, array, etc. filled with data.


#For uniformity and compatibility, each method returns a list of
# the desired data type(s).


#NOTE: currently, only positive numbers are generated. 

library('stringi')

# Integer generation
# getInt randomly generates integer numbers.
#
# param   size      the number of requested integers
# param   min       minimum integer value
# param   max       maximum integer value
# param   repl      should the sample generation use replacement?  
# return            list of integers
getInt <- function(size = 1, min = 1, max = 10000, repl= FALSE ){
  ints <- vector(mode = "list", length = size)
  if((max - min) < size){
    print("The size of the requested list is larger than the pool of available integers. Forcing replacement...")
    repl = TRUE
  }
  
    #populate the list with sampled values
    for(e in (1:size)){
      ints[[e]] <- sample(min:max, 1, replace = repl) 
    }
  
    return(ints)
}

#Real Number Generation
# getReal() randomly generates real numbers
#
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
# getComplex() randomly generates complex numbers
#
# param   size      The number of requested complex nubmers
# param   min       The minimum real or complex coefficient
# param   max       The maximum real or complex coefficient
# param   type      int(1:2) The data type of the coefficient 
#                   (1: int, 2: real)
#return             List of complex numbers
getComplex <- function(size = 1, min = 1, max = 10000, type = 1){
  complexNums <- list()
  
  for(i in(1:size)){ #integer type coefficients
    z <- 0
    if(type==1){
      z <- complex(real = getInt(1, min = min, max = max), imaginary = getInt(1, min = min, max = max))
    }
    else if(type ==2){ #real type coefficients
      z <- complex(real = getReal(1, min = min, max = max), imaginary = getReal(1, min = min, max = max))
    }
    complexNums <- c(z, complexNums)
  }
    

  return(complexNums)
}
  

# Character Generation
# getCharString pseudo-randomly generates a string of alphabetical 
# characters. Although this function's purpose to to return a list
# of single character vectors, the option is available to return
# a list of strings by specifying a length greater than 1.
#
# param   size      Number of character 'strings' to return
# param   length    Length of the character string(s)
#
# return            List of character strings
getCharString <- function(size = 1, length = 1){
  chars <- list()
  
  for(i in (1:size)){
    #generates 1 random string
    x <- stri_rand_strings(1,length, pattern = "[A-Za-z]")
    chars <- c(x, chars) #add the string to list
  }
  return(chars)
}


#String Generation
# getString does not actually generate the Strings, but rather
# samples one of six csv files and returns the sampled strings.
#
# param size      Number of Strings to return
# param cat       The category of strings (1:6)
#                 (1: all, 2: Country names, 3: City Names
#                 4: Male Names, 5: Female Names, 6: Names)
getString <- function(size = 1, cat = 6){
  strings <- list()
  
  # generate file and read the appropriate file
  fName <- paste("utils/StringData/", cat, ".csv", sep = "")
  data <- readLines(fName)
  
  # populate output with sample from the appropriate data
  for(x in (1:size)){
    i <- sample.int(length(data), 1)
    d <- data[i]
    strings <- c(d, strings)
    
  }
  return(strings)
}





#Set Generation
# getSets generates n sets of m elements of type x.
#
# param   n       Number of sets to return
# param   m       Number of elements in the set
# param   x       Data type of elements in the sets
#                 (1: Ints, 2: Real, 3: Complex, 
#                  4: Char, 5: String, 6: Mixed)
#
# returns         list of sets


getSets <- function(n = 2, m = 5, x = 1){
  sets <- vector(mode = "list", length = n)
  for(s in (1:n)){
    if(x == 1){           # Integers
      sets[[s]] <- getInt(size = m, min=1, max=20)
    }
    else if(x == 2){           # Reals
      sets[[s]] <- getReal(size = m, max = 20)
    }
    else if(x == 3){           # Complex
      sets[[s]] <- getComplex(size = m, max = 10)
    }
    else if(x == 4){           # Chars
      sets[[s]] <- getCharString(size = m)
    }
    else if(x == 5){           # Strings
      sets[[s]] <- getString(size = m, cat = 6) #defaulted content to "names"
    }
    else if(x == 6){           # Mixed 
      #TODO: this nests inside another list. lets find a more elegant way to do this.
      sets[[s]] <- getSets(1, m, x = sample(1:5, 1, replace = FALSE))
    }
  }
  return(sets)
}


#Value Generation
# getValue generates 1 value of type x.
#
# param   x       Data type of elements in the sets
#                 (1: Ints, 2: Real, 3: Complex, 
#                  4: Char, 5: String, 6: Mixed)
# param   min     The minimum numeric value for numeric types
# param   max     The maximum numeric value for numeric types
# param   cat     The category of string for string type
#
# returns         a single value of type x
getValue <- function(x = 1, min = 1, max = 20, cat = 6){
  value <- NULL
    if(x == 1){           # Integers
      value <- getInt(size = 1, min=min, max=max)
    }
    else if(x == 2){           # Reals
      value <- getReal(size = 1, max = max)
    }
    else if(x == 3){           # Complex
      value <- getComplex(size = 1, max = max)
    }
    else if(x == 4){           # Chars
      value <- getCharString(size = 1)
    }
    else if(x == 5){           # Strings
      value <- getString(size = 1, cat = 6) #defaulted content to "names"
    }
    else if(x == 6){           # Mixed 
      value <- getSets(1, 1, x = sample(1:5, 1, replace = FALSE))
    }
  return(value)
}
