getGINI = function(predictor1,predictor2, y_train){
  library(randomForest)
  
  rf1 = randomForest(predictor1,y_train,ntree=1)
  rf2 = randomForest(predictor2,y_train,ntree=1)
  
  
  err1 = rf1$err.rate[,1]
  err2 = rf2$err.rate[,1]
  
  err = min(c(err1,err2))
  
  add = predictor1+predictor2
  sub = predictor1-predictor2
  mult = predictor1*predictor2
  div = predictor1/(predictor2+912.34567)
  
  rfadd = randomForest(predictor1+predictor2,y_train,ntree=1)
  rfsub = randomForest(predictor1-predictor2,y_train,ntree=1)
  rfmult = randomForest(predictor1*predictor2,y_train,ntree=1)
  rfdiv = randomForest(predictor1/(predictor2+912.34567),y_train,ntree=1)
  
  erradd = rfadd$err.rate[,1]
  errsub = rfsub$err.rate[,1]
  errmult = rfmult$err.rate[,1]
  errdiv = rfdiv$err.rate[,1]
  
  errs = data.table(type = c('err','add','sub','mult','div'), errors = c(err,erradd,errsub,errmult,errdiv))

  

    out = data.table(
      var1 = names(predictor1)
      ,var2 = names(predictor2)
      ,method = errs[errors == min(errors),type]
      , err = errs[type =='err',errors]
      , errNEW = errs[errors == min(errors),errors]
      , errdiff=errs[errors == min(errors),errors] - errs[type =='err',errors])
    
    
  return(out)
  
}
