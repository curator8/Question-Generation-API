# Author:               Trevor Strobel
# File:                 utils.R
# Date:                 4/11/21
# Last Edited:          4/11/21



# formatAsSet(str) formats a List as a string representing
# a set in set notation for display with MathJax.
# TODO: there is currently an issue with having to escape the
# escape character. This causes MathJax to not render the '\\{'
# or '\\}'
formatListAsSet <- function(inputList){

    #format as string
    result <- paste(c(inputList), collapse=', ')

    #insert prefix and postfix. escape character nonsense involved here. 
    #prefix <- r"($$\{)"
    #postfix <- r"(\}$$)"
    #result <- c(prefix, result, postfix)
    return(result)
}