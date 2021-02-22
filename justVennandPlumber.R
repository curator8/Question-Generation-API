library(venn)
library(plumber)



#* @get /getAVenn
getAVenn <- function() {

  venn("~B", snames = "A, B",sncs = 4)  #this is the line that actually draws the plot in the buffer. 

  
}

getAVenn()

