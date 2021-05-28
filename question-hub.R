# Author:         Trevor Strobel
# File:           question-hub.R
# Date:           4/11/2021

library(plumber)
source("SetTheory/set-relation.R")
#this file acts as a routing hub to fetch questions for use in the ISPeL system.




#The following is pretty unsafe and is only planned to be used during early development. 
#It allows for Cross Origin Resource Sharing from any client. 
# TODO: I'm not going to worry about this for the time being as the API is only available on
# the server. The port is not exposed to the network, so it shouldn't be an issue. 
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}


#* @post /getSetUnion
getSetUnionQ <- function(qType = 1, qDifficulty = 1, dataType = 1 ){
  qTopic <- "setUnion"
  qFormat <- "1"
  output <- "If you're seeing this, Question Generation isn't working properly."
  #question specifics are returned as a list
  question <- list()
  
  
  if(qType == 1){
    question <- getSetUnionMC(dType = dataType)
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
  }
  
  return(output)
}


#* @post /getSetIntersect
getSetIntersect <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setIntersect"
  qFormat <- "1"
  output <- "If you're seeing this message, question generation isn't working properly."
  
  #question details are returned as a list. 
  question <- list()
  
  if(qType == 1){
    question <- getSetIntersectMC(dType = dataType)
    
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
  }
  
  return(output)
}

#* @post /getAsymDiff
getAsymDiff <- function(qType = 1, qDifficulty = 1) {
  qTopic <- "asymecticDifference"
  qFormat <- "1"
  output <- "If you're seeing this message, question generation isn't working properly."
  
  if(qType == 1){
    question <- getAsymDiffMC()
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
  }
  
  return (output)
}

