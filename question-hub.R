# Author:         Trevor Strobel
# File:           question-hub.R
# Date:           4/11/2021

library(plumber)
library(jsonlite)


source("SetTheory/set-relation.R")



#This file describes the generation of multiple choice questions
# on the topic of Sets.


options_plumber(
  port = 3157
)


#The following is pretty unsafe and is only planned to be used during early development. 
#It allows for Cross Origin Resource Sharing from any client. 
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* @post /getSetUnion
getSetUnionQ <- function(qType = 1, qDifficulty = 1 ){
  qTopic <- "setUnion"
  qFormat <- "1"
  output <- "If you're seeing this, Question Generation isn't working properly."
  #question specifics are returned as a list
  question <- list()
  
  if(qType == 1){
    question <- getSetUnionMC()
    output <- list(topic = qTopic, type = qType, format = qFormat, difficulty = qDifficulty, question = question)
  }
  
  return(output)
}


#* @post /getSetIntersect
getSetIntersect <- function(qType = 1, qDifficulty = 1) {
  qTopic <- "setIntersect"
  qFormat <- "1"
  output <- "If you're seeing this message, question generation isn't working properly."
  
  #question details are returned as a list. 
  question <- list()
  
  if(qType == 1){
    question <- getSetIntersectMC()
    
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

