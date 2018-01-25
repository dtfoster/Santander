evaluator = function(act,pred,metric){

  
  #act is a vector
  #pred is a data table
  
  if (metric == 'MultiLogLoss'){
    response_grid = data.frame(model.matrix(~act-1))
    out = MultiLogLoss(response_grid,pred)
  }
  
  if (metric == 'RMSE'){

    out = RMSE(act,pred[[response_col]])
  }
  
  if (metric == 'NormalizedGini'){
    
    out = NormalizedGini(act,pred[[response_col]])
  }
  
  if (metric == 'my.auc'){
    
    out = my.auc(as.integer(act)-1,pred[[response_col]])
  }
  
  if (metric == 'perc_correct'){
    
    out = perc_correct(as.integer(act)-1,as.factor(pred[[response_col]]))
  }
  
  return (out)
  
  
}