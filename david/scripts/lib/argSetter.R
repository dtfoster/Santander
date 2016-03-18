library(xgboost)


XSetter = function(m,X){
  out = list()
  
  if (m[['model_type']] %in% c('randomForest','multinom','lm')){
    out[['x']] = X
  }else if(m[['model_type']] %in% c('cv.glmnet')){
    out[['x']] = as.matrix(X)
    #print(head(out[['x']]))
    }else if (m[['model_type']] %in% c('h2o.deeplearning')){
      out[['newdata']] = as.h2o(X)
      
      
      }else if (m[['model_type']] %in% c('xgb.train')){
    X_new = data.matrix(X)
    out[['data']] =xgb.DMatrix(X_new)
  }
  #cat ('Finished setting X arguments...\n')
  return (out)
}

ySetter = function(m,y){
  out = list()
  

    if (response_type =='continuous'){
      out[['y']] = as.numeric(y)
    }else {
      out[['y']] = as.factor(y)
    }
  #cat ('Finished setting y arguments...\n')
  return (out)
}

formulaSetter = function(m,X,y){

  out = list()
  
  if (m[['model_type']] == 'xgb.train'){
    if (response_type =='continuous'){
      label = as.numeric(y)
    }else{
      label = as.integer(y) - 1
    }
    #print(length(label))
    
    
    samp = sample(1:nrow(X),floor(nrow(X)/8)) #1:3000 #
    X_new = data.matrix(X)
    data = xgb.DMatrix(X_new[-samp,], label=label[-samp])
    eval = xgb.DMatrix(X_new[samp,], label=label[samp])

    rm(X_new)
    gc()
    
    out[['data']] = data
    out[['watchlist']] = list(eval = eval, train = data)
    if (response_type =='continuous'){
      out[['feval']] = evalgini
    }else if (response_type == 'discrete'){
      out[['feval']] = MultiLogLoss_XG
      
    }else if (response_type == 'binary'){
      if (binary_type =='absolute'){
      out[['feval']] = perc_correct_XG 
      }else{
      out[['feval']] = my.auc_XG
      }
      
    }
    
  }else if(m[['model_type']] %in% c('h2o.deeplearning')){
    
  
  out[['training_frame']] = as.h2o(cbind(data.frame(response = y),X))
  out[['x']] = 2:ncol(out[['training_frame']])
  out[['y']] = 1
  }else{
    out[['formula']] = as.formula('response~.')
    out[['data']] = cbind(X,data.frame(response = y))
    
  }
  

  
  #cat ('Finished setting formula arguments...\n')
  return (out)
}

