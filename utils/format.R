# Author:               Trevor Strobel (strobelt09), Aleksei Vilkomir (avilk)
# File:                 utils.R
# Date:                 4/11/21

library(stringr)

# formatAsSet(str) formats a List as a string representing
# a set in set notation for display with MathJax.
# TODO: there is currently an issue with having to escape the
# escape character. This causes MathJax to not render the '\\{'
# or '\\}'
formatListAsSet <- function(inputList){

    #format as string
    result <- paste(c(inputList), collapse=', ')

    #insert prefix and postfix. escape character nonsense involved here. 
    finalResult <- paste("\\$\\{", result," \\}\\$")
    return(finalResult)
}

#Function to correctly format inner sets so that there are no display issues.
formatPartitionAsSet <- function(inputList){
    
    #format as string
    result <- paste(c(inputList), collapse=', ')
    
    #insert prefix and postfix. escape character nonsense involved here. 
    finalResult <- paste("\\{", result," \\}")
    return(finalResult)
}


# insertSetQStrings takes a list of sets and appends strings to the list
#  such that the final list is as follows:
# 
#  'Let \\; A=' [a set of numbers]
#  'and \\; B=' [a set of numbers] 
#  'be \\; two \\; sets.'
insertSetQStrings <- function(sets) {
    nsets <- list()
    #nsets[1] <- 'Let A and B be two sets. What is \\$A\\cup B\\$?'
    nsets[1] <- paste('\\$A=\\$', sets[1])
    nsets[2] <- paste('\\$B=\\$',sets[2])

    return(nsets)
}

# insertSetQStrings takes a list of sets and appends strings to the list
#  such that the final list is as follows:
# 
#  'Let \\; A=' [a set of numbers]
#  'and \\; B=' [a set of numbers]
#  'and \\; C=' [a set of numbers]
#  'be \\; three \\; sets.'
insertSet3Strings <- function(sets) {
    nsets <- list()
    #nsets[1] <- 'Let A and B be two sets. What is \\$A\\cup B\\$?'
    nsets[1] <- paste('\\$A=\\$', sets[1])
    nsets[2] <- paste('\\$B=\\$',sets[2])
    nsets[3] <- paste('\\$C=\\$',sets[3])
    return(nsets)
}

# insertSetRStrings takes a single set and appends strings to the set
#  such that the final list is as follows:
# 
#  'Let \\; A=' [a set of numbers]
#  'be \\; a \\; set.'
insertSetRStrings <- function(sets) {
    nsets <- list()
    nsets[1] <- paste('\\$A=\\$', sets[1])
    
    return(nsets)
}