# Authors:         Trevor Strobel
# File:           question-hub.R
# Date:           4/11/2021

library(plumber)
source("SetTheory/set-relation.R")
 source("venn/venn-gen2.R") #new cleaner test for image transport. answers are diagrams



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
    question <- getSetUnionMC(dType = dataType, difficulty = qDifficulty)
    
    
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
getAsymDiff <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "asymecticDifference"
  qFormat <- "1"
  output <- "If you're seeing this message, question generation isn't working properly."
  
  if(qType == 1){
    question <- getAsymDiffMC(dType = dataType)
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
  }
  
  return (output)
}

#* @post /getSetComplement
#* @param qType   The data type.
getSetComplement <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setComplement"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    question <- getSetComplementMC(dType = dataType)
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
  }
  
  return(output)
}

#* @post /getSetEquality
getSetEquality <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setEquality"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    question <- getSetEqualityMC(dType = dataType)
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
    
  }
  
  return(output)
}

#* @post /getSetCardinality
getSetCardinality <- function(qType = 1, qDifficulty = 1, dataType = 1) {
  qTopic <- "setCardinality"
  qFormat <- "1"
  #Error Message
  output <- "If you're seeing this message, question generation isn't working properly."
  
  question <- list()
  
  #checks for question type, calls function, and formats output
  if (qType == 1) {
    question <- getSetCardinalityMC(dType = dataType)
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
    
  }
  
  return(output)
}

#* @post /setExpression
setExpressionQ <- function(qType = 1, qDifficulty = 1) {
 qTopic <- "setExpressions"
 qFormat <- 2
 #Error Message
 output <- "If you're seeing this message, question generation isn't working properly."
 
 question <- list()
  
 #checks for question type, calls function, and formats output
 if (qType == 1) {
   question <- getSetExpressionMC()
   output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
 }
 return(output)
} 




