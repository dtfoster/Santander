write_rds <- function(x, path, compress = c("none", "gz", "bz2", "xz"), ...) {
  
  compress <- match.arg(compress)
  con <- switch(compress,
                none = file(path, ...),
                gz   = gzfile(path, ...),
                bz2  = bzfile(path, ...),
                xz   = xzfile(path, ...))
  on.exit(close(con), add = TRUE)
  saveRDS(x, con)
}


get.sizes = function(){

  print(sort( sapply(ls(globalenv()),function(x){object.size(get(x))}),decreasing=TRUE))
  
}

library(caret)


response_class = function(response){
  if (response_type == 'continuous'){
    out = as.numeric(response)
  }else{
    out = as.factor(response)
  }
  return (out)
}

write_scores = function(m){
  
  modeltype = m$model_type
  
  if (modeltype =='xgb.train'){
    csv_row = list(
      datetime = m$time_stamp
      ,dataset = m$dataset
      ,score = m$score
      ,train_score = m$train_score
      ,seed = m$seed
      ,eta =  m$args$param$eta
      ,gamma = m$args$param$gamma
      ,max_depth = m$args$param$max_depth
      ,min_child_weight = m$args$param$min_child_weight
      ,subsample = m$args$param$subsample
      ,colsample_bytree = m$args$param$colsample_bytree
      ,num_parallel_tree = m$args$param$num_parallel_tree
      ,objective = m$args$param$objective
      ,nrounds = m$args$nrounds
      ,early.stop.round = m$args$early.stop.round
    )
    
    filename = './scores/xgb_scores.csv'
  }

  if (modeltype =='randomForest'){
    csv_row = list(
      datetime = m$time_stamp
      ,dataset = m$dataset
      ,score = m$score
      ,train_score = m$train_score
      ,ntree = m$args$ntree
      #,sampsize = m$args$sampsize
      ,nodesize = m$args$nodesize
    )
    

    
    filename = './scores/rf_scores.csv'

  }
  
  if (modeltype =='lm'){
    csv_row = list(
      datetime = m$time_stamp
      ,dataset = m$dataset
      ,score = m$score
      ,train_score = m$train_score
    )
    
    filename = './scores/lm_scores.csv'
    
  }  
  
  if (modeltype =='cv.glmnet'){
    csv_row = list(
      datetime = m$time_stamp
      ,dataset = m$dataset
      ,score = m$score
      ,train_score = m$train_score
      ,family = m$args$family
      ,alpha = m$args$alpha
      ,nlambda = m$args$nlambda
      ,nfolds = m$args$nfolds
    )
    
    filename = './scores/glm_scores.csv'
    
  }
  
  if (modeltype =='h2o.deeplearning'){
    csv_row = list(
      datetime = m$time_stamp
      ,dataset = m$dataset
      ,score = m$score
      ,train_score = m$train_score
      ,activation = m$args$activation
      ,input_dropout_ratio = m$args$input_dropout_ratio
      ,hidden_dropout_ratios = paste(m$args$hidden_dropout_ratios,collapse=' ')
      ,hidden = paste(m$args$hidden,collapse=' ')
      ,epochs = m$args$epochs
      ,l1 = m$args$l1
      ,l2 = m$args$l2
    )
    
    filename = './scores/deep_scores.csv'
    
  }  
  
  col_flag = !file.exists(filename)
  write.table(csv_row,file = filename,append=TRUE,row.names=FALSE,col.names=col_flag,sep=',')

  
}



inverseYJ = function(data,lambda){
  
  
  
  
  
  
  
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
