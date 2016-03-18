trainer=function(m,X,y){
  
  model_type=m[['model_type']]
  
  if (model_type %in% c('randomForest','cv.glmnet')){

    args = c(m[['args']], XSetter(m,X), ySetter(m,y))
  }else{

    args = c(m[['args']], formulaSetter(m,X,y))
  }
  set.seed(m[['seed']])
  model = do.call(model_type,args)
  return (model)
}