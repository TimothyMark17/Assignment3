rankhospital<- function(state, outcome, num = "best")   {
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!any(state == outcome1$State))   {
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE)   {
    stop(print("invalid outcome"))
  }
  outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack")    {
    colnum <- 11
  }
  else if (outcome == "heart failure")   {
    colnum <- 17
  }
  else  {
    colnum <- 23
  }
  outcome2[ ,colnum] <- as.numeric(outcome2[ ,colnum])
  outcome3 <- outcome2[order(outcome2[ ,colnum],outcome2[,2]), ]
  outcome3 <- outcome3[(!is.na(outcome3[ ,colnum])),]
  if(num == "best")   {
    num <- 1
  }            
  else if (num == "worst")  {
    num <- nrow(outcome3)
  }      
  return(outcome3[num,2])
}