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
    prefix <- r"(\\{)"
    postfix <- r"(\\})"
    result <- str_c(prefix, result, postfix)
    return(result)
}


# insertSetQStrings takes a list of sets and appends strings to the list
#  such that the final list is as follows:
# 
#  'Let \\; A=' [a set of numbers]
#  'and \\; B=' [a set of numbers] 
#  'be \\; two \\; sets.'
insertSetQStrings <- function(sets) {
    sets[1] <- paste('Let \\; A=', sets[1])
    sets[2] <- paste('and \\; B=', sets[2])
    sets[3] <- 'be \\; two \\; sets.'

    return(sets)
}