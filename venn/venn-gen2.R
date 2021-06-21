# Author:       Trevor Strobel
# Date:         6/17/2021
# File:         venn-gen2.R

library(venn)
library(base64enc)
library(plumber)  #for testing


#gets a list of  set expressions
getSetExpressions <- function(){
  
  setExprs <- sample(list("~A~B", "AB", "A~B", "~AB"),4, replace = FALSE)
  return(setExprs)
}

# takes a set expression string and returns a b64 encoding of the venn diagram
# that represents that expression.
getBase64FromExpr <- function(expr){
  png(file="set_diag_tmp.png")
  venn(expr,sncs = 4)  #this is the line that actually draws the plot in the buffer. 
  dev.off() #save the plot to disk
  
  encodedVenn <- base64encode("set_diag_tmp.png")
  encodedVenn <- paste0("data:image/png;base64,", encodedVenn)
  
  return(encodedVenn)
}


#makes a set expression question with venn diagrams as answer choices
getSetExpressionMC <- function(){
  questionText <- ('Which diagram represents the following set expression?')
  
  #create list of expressions
  setExprs <- getSetExpressions()
  
  #set question contents
  sourceExpr <- setExprs[[1]]
  
  #correct answer expression
  correct <- sourceExpr
  
  #make distractors
  distractors <- setExprs
  distractors[[1]] <-NULL

  #encoding correct and disctractors to b64 png  
  correct <- getBase64FromExpr(correct[[1]])
  #distractor encoding
  counter <-1
  for (d in distractors){
    distractors[counter] <- getBase64FromExpr(d)
    counter <- counter +1
  }
  
  
  #format for output
  questionContents <- c(questionText, sourceExpr)
  
  toSend <- list(content = questionContents, correct = correct, distractors = distractors)
}


x <- getSetExpressionMC()

print(getBase64FromExpr("A~B"))


