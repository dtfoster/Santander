getBlendModels = function(){

  
library(randomForest)
library(nnet)
library(glmnet)
library(xgboost)
  
models = list()


n = 1

models[[n]] = list()
models[[n]][['name']] = 'LM1'
models[[n]][['model_type']] = 'lm'
models[[n]][['args']] = list()
names(models[[n]][['args']]) = c()




n = n + 1

models[[n]] = list()
models[[n]][['num']] = n
models[[n]][['name']] = 'RF2'
models[[n]][['model_type']] = 'randomForest'
models[[n]][['args']] = list(100
                             ,1
                             ,10000
                             ,5
)
names(models[[n]][['args']]) = c('ntree'
                                 ,'verbose'
                                 ,'sampsize'
                                 ,'nodesize'
                                 #,mtry
                                 #,nodesize
)





#for ( max_depth in c(3,5,7)){
 # for ( eta in c(0.01)){
 #   for ( min_child_weight in c(1,100)){
 #   for ( colsample_bytree in c(0.5,0.7)){
      
      
      # for ( max_depth in c(6,7,8,9)){
      #   for ( eta in c(0.001,0.005)){
      
      # for ( min_child_weight in c(10,100,1000)){
      #   for ( num_parallel_tree in c(20,50)){
      # for ( subsample in c(0.7,0.85,1)){
      #     for ( colsample_bytree in c(0.5,0.6)){
      
      
       eta = 0.01
      gamma = 0
       max_depth = 5
      min_child_weight = 1
      subsample = 0.7
      colsample_bytree = 0.8
       num_parallel_tree = 1
      objective = 'binary:logistic'
      seed = 1
      
      
      n = n + 1
      
      models[[n]] = list()
      models[[n]][['num']] = n
      models[[n]][['name']] = paste0('XGB',n)
      models[[n]][['model_type']] = 'xgb.train'
      models[[n]][['numrows']] = 0
      models[[n]][['numcols']] = 0
      models[[n]][['seed']] = seed
      models[[n]][['args']] = list(list('eta'=eta
                                        , 'gamma'= gamma
                                        ,'max_depth'=max_depth
                                        ,'min_child_weight'= min_child_weight
                                        ,'subsample'= subsample
                                        ,'colsample_bytree'= colsample_bytree
                                        ,'num_parallel_tree' = num_parallel_tree
                                        , 'objective'=objective
                                        #,'eval_metric'= 'rmse'
                                        #, 'num_class'=9
      ),1,TRUE,100000,100,1)
      names(models[[n]][['args']]) = c('param','verbose','maximize','nrounds','early.stop.round','print.every.n')
      
   # }
 # }
#}
#}

      
      n = n+1
      
      
      models[[n]] = list()
      models[[n]][['name']] = 'GLM1'
      models[[n]][['model_type']] = 'cv.glmnet'
      models[[n]][['numrows']] = 0
      models[[n]][['numcols']] = 0
      models[[n]][['seed']] = 853
      models[[n]][['args']] = list('binomial' 
                                   ,1
                                   ,100
                                   ,10
                                   )
      names(models[[n]][['args']]) = c('family','alpha' ,'nlambda','nfolds' )
      




return (models)

}