library(pROC)

perc_correct<-function(act,pred){

  
  out = as.numeric(length(which(act==pred))) / length(act)

  return(out)
  
  
}

perc_correct_XG<-function(pred, dtrain){
  act <- getinfo(dtrain, "label")
  pred = as.factor(round(pred))
  return (list(metric = "perc_correct", value = perc_correct(act,pred)))
}


my.auc <- function(act, pred){
  
  return(auc(act,pred))
  
}

my.auc_XG <- function(pred, dtrain){
  act <- getinfo(dtrain, "label")
  return (list(metric = "auc", value = my.auc(act,pred)))
  
}



MultiLogLoss <- function(act, pred)
{
  
  
  
  eps = 1e-15;
  #print(head(act))
  #print(head(pred))
  nr <- nrow(pred)
  pred[pred<eps]=eps # = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred[pred>1-eps]=1-eps #pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  
  pred = as.data.frame(pred)

  ll = sum(act*log(pred) )
  ll = ll * -1/(nrow(act))      
  return(ll);
}

MultiLogLoss_XG <- function(pred,dtrain)
{
  act <- as.factor(getinfo(dtrain, "label"))
 # print(head(act))
  response_grid_act = data.frame(model.matrix(~act-1))
  #print(response_grid_act)
  response_grid_pred = data.frame(matrix(pred,ncol=num_classes,byrow=TRUE))
  
  
  

  #cat('\n')
 # print(head(response_grid_act))
 # print(head(response_grid_pred))
  err <- MultiLogLoss(response_grid_act,response_grid_pred)
  
  return(list(metric = "MultiLogLoss", value = err))
}


RMSE <- function(act, pred)
{
  n = length(act)
  out = sqrt((sum((act-pred)^2))/n)
  return(out);
}

#"NormalizedGini" is the other half of the metric. This function does most of the work, though
SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  #df
  df$random = (1:nrow(df))/nrow(df)
  #df
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  #print(df)
  return(sum(df$Gini))
}

NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}


evalgini <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- NormalizedGini(as.numeric(labels),as.numeric(preds))
  return(list(metric = "Gini", value = err))
}
