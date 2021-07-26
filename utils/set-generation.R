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
    x <- sample(min:max, size = size, replace = repl)
    for(e in (1:size)){
      ints[[e]] <- x[[e]]
    }
  
    return(ints)
}

x <- getInt(5, max = 10)
print(x)

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

#getSeqSets
# getSeqSets generates 1 sequential set between a given range of from and to values.
#
# param   Sequence    Currently only one sequence available
# param   from        Starting integer value
# param   to          Ending integer value
# param   membersType Member type of values. Currently only available for integers.
#                     (1: Ints)
#
# returns          vector containing set

getSeqSets <- function(Sequence = 1, from = sample(1:10, 1, replace = FALSE), to = sample(11:20, 1, replace = FALSE), membersType = 1) {
  sets <- vector()
  if (Sequence == 1) {
    sets <- seq(from, to, by = 1)
  }
  return(sets)
}


#Set Notation Generation
# getSetNotations generates 1 set from a starting border to an ending border. 
# Allows user to specify whether starting and ending values are included
#
# param   leftIncl         Whether left border is included in set 
# param   rightIncl        Whether right border is included in set
# param   leftBorder       Minimum value of sequential set.
# param   rightBorder      Maximum value of sequential set.
# param   membersType      Currently only Ints and Real Numbers are supported.
# param   notation         Three notations are available, 1 = Roster, 2 = SetBuilder, 3 = Interval
#                 (1: Ints, 2: Real, 3: Complex, 
#                  4: Char, 5: String, 6: Mixed)
#
# returns         list of sets

getSetNotations <- function(leftIncl = FALSE, rightIncl = FALSE, leftBorder = sample(1:10, 1, replace = FALSE), rightBorder = sample(11:20, 1, replace = FALSE), membersType = 1, notation = 2, format = TRUE) {
  answerSet <- vector(mode = "list", 1)
  if (membersType == 1) {
    if (leftIncl == TRUE) {
      leftBorder4Generation <- leftBorder
    }
    else {
      leftBorder4Generation <- leftBorder + 1
    }
    if (rightIncl == TRUE) {
      rightBorder4Generation <- rightBorder
    }
    else {
      rightBorder4Generation <- rightBorder - 1
    }
    membersType <- "ℤ"
  }
  if (membersType == 2) {
    if (leftIncl == TRUE) {
      leftBorder4Generation <- leftBorder
    }
    else {
      leftBorder4Generation <- leftBorder + 0.1
    }
    if (rightIncl == TRUE) {
      rightBorder4Generation <- rightBorder
    }
    else {
      rightBorder4Generation <- rightBorder - 0.1
    }
    membersType <- "ℝ"
  }
  resultSet <- getSeqSets(Sequence = 1, leftBorder4Generation, rightBorder4Generation, membersType)
  
  if (notation == 1) {
    resultSetString <- resultSet
    answerSet[[2]] <- resultSetString
    if (format == TRUE) {
    answerSet[[2]] <- formatListAsSet(answerSet[[2]])
    }
    else {
      answerSet[[2]] <- formatPartitionAsSet(answerSet[[2]])
    }
  }
  else if (notation == 2) {
    resultSetString <- paste('x ∈', membersType, '|', leftBorder, (if(leftIncl == TRUE) '≤ x' else '< x')  
                             , (if(rightIncl == TRUE) '≤' else '<'), rightBorder)
    answerSet[[2]] <- resultSetString
    if (format == TRUE) {
      answerSet[[2]] <- formatListAsSet(answerSet[[2]])
    }
    else {
      answerSet[[2]] <- formatPartitionAsSet(answerSet[[2]])
    }
  }
  else {
    resultSetString <- paste((if(leftIncl == 1) '[' else '(' ), leftBorder, ',', rightBorder, (if(rightIncl == 1) ']' else ')'))
    answerSet[[2]] <- resultSetString
  }
  
  answerSet[[1]] <- resultSet
  return(answerSet)
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
