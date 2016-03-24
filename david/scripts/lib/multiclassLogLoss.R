

MultiLogLoss <- function (data, lev = NULL, model = NULL)
{
  act=model.matrix(~pred-1, data)
  colnames(act) <- gsub('pred', '', colnames(act))
  pred = as.matrix(data[,-(1:2)])
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) )
  ll = ll * -1/(nrow(act))  
  names(ll) = 'logLoss'
  print(ll)
  return(ll);
}






multiClassSummary <- function (data, lev = NULL, model = NULL){
 
  #print (data)
  #Load Libraries
  require(Metrics)
  require(caret)
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("levels of observed and predicted data do not match")
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    eps = 1e-15
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs <- ifelse(data[, "obs"] == class, 1, 0)
    prob <- data[,class]
    print(length(pred))
    print(pred)
    print(length(obs))
    print(obs)
    print(prob)
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, eps), 1-eps)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    
    names(prob_stats) <- c('ROC', 'logLoss')
    print(prob_stats)
    return(prob_stats)
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  print(prob_stats)
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  print(CM)
  print(CM$byClass)
  print(CM$overall)
  print('here')
  #Aggregate and average class-wise stats
  #Todo: add weights
  class_stats <- cbind(CM$byClass, prob_stats)
  class_stats <- colMeans(class_stats)
  print(class_stats)
  #Aggregate overall stats
  overall_stats <- c(CM$overall)
  print(overall_stats)
  #Combine overall with class-wise stats and remove some stats we don't want
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull',
                                       'Prevalence', 'Detection Prevalence')]
  #Clean names and return
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  print (stats)
  return(stats)
}