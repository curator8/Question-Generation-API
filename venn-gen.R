library(httr)
library(jsonlite)
library(venn)
library(plumber)


#*echo back the input
#* @param msg     the message to echo
#* @get /echo
echoMsg <- function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* @param  n    The number of sets in the venn diagram
#* @get /nset
#* @serializer png

getEmptyVenn <- function(n) {
  return(venn(n))
}


venn("A~BC")