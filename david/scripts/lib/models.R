getModels = function(){

  
  #library(randomForest)
  #library(nnet)
  #library(glmnet)
  library(xgboost)
  #library(h2o)


models = list()

 n = 1
 
 models[[n]] = list()
 models[[n]][['num']] = n
 models[[n]][['name']] = 'RF2'
 models[[n]][['model_type']] = 'randomForest'
 models[[n]][['numrows']] = 0 #20000
 models[[n]][['numcols']] = 0
 models[[n]][['seed']] = 1
 models[[n]][['args']] = list(5
                              ,100
                              ,1 #/ 10000  #nrow(X_train)
                              ,3 #10
                              #,1000
                              ,TRUE
                              )
 names(models[[n]][['args']]) = c('do.trace'
                                  ,'ntree'
                                  ,'nodesize'
                                  ,'mtry'
                                  ,'importance'
                                  #,'sampsize'
                                  
 )
 
 
 
 
 n = n+1
 
 
 models[[n]] = list()
 models[[n]][['name']] = 'GLM1'
 models[[n]][['model_type']] = 'cv.glmnet'
 models[[n]][['numrows']] = 0
 models[[n]][['numcols']] = 0
 models[[n]][['seed']] = 855
 models[[n]][['args']] = list('gaussian' #gaussian (for continuous), multinomial (for multi category) , binomial (for binary) 
                              ,1
                              ,100
                              ,10
 )
 names(models[[n]][['args']]) = c('family','alpha' ,'nlambda','nfolds')
 
 
 
 
 
 
 #for ( max_depth in c(5,8,11)){
#   for ( eta in c(0.005,0.01,0.02)){

# for ( min_child_weight in c(1, 10,100)){
#   for ( num_parallel_tree in c(20,50)){
# for ( subsample in c(0.6,0.8)){
#     for ( colsample_bytree in c(0.5,0.7)){
#       for ( objective in c('rank:pairwise' ,'reg:linear')){
 #for ( seed in c(1:10)){
       

 eta = 0.02
 gamma = 0
 max_depth = 6
 min_child_weight = 1
 subsample = 0.9
 colsample_bytree = 0.85
 num_parallel_tree = 1
 objective = 'binary:logistic'#'multi:softprob' #'binary:logistic' #

 
 
 n = n + 1
 
models[[n]] = list()
models[[n]][['num']] = n
models[[n]][['name']] = paste0('XGB',n)
models[[n]][['model_type']] = 'xgb.train'
models[[n]][['numrows']] = 0
models[[n]][['numcols']] = 0
models[[n]][['seed']] = 7#seed
models[[n]][['args']] = list(list('eta'=eta
                                  , 'gamma'= gamma
                                  ,'max_depth'=max_depth
                                  ,'min_child_weight'= min_child_weight
                                  ,'subsample'= subsample
                                  ,'colsample_bytree'= colsample_bytree
                                  ,'num_parallel_tree' = num_parallel_tree
                                  , 'objective'=objective
                                  #,'eval_metric'= 'auc'
                                  #, 'num_class'=num_classes
                                  
),1,TRUE,100000,100,1)
names(models[[n]][['args']]) = c('param','verbose','maximize','nrounds','early.stop.round','print.every.n')

#}
#    }
#  }
#  }
#}








#localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

## Start a local cluster with 2GB RAM
#  localH2O <<- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
#                        max_mem_size = '2g')





#n = n + 1

#rand_activation <- c("TanhWithDropout", "RectifierWithDropout")[sample(1:2,1)]
#rand_numlayers <- sample(2:5,1)
#rand_hidden <- c(sample(10:50,rand_numlayers,T))
#rand_l1 <- runif(1, 0, 1e-3)
#rand_l2 <- runif(1, 0, 1e-3)
#rand_dropout <- c(runif(rand_numlayers, 0, 0.6))
#rand_input_dropout <- runif(1, 0, 0.5)




#models[[n]] = list()
#models[[n]][['num']] = n
#models[[n]][['name']] = paste0('Deep',n)
#models[[n]][['model_type']] = 'h2o.deeplearning'
#models[[n]][['numrows']] = 0
#models[[n]][['numcols']] = 0
#models[[n]][['seed']] = 853
#models[[n]][['args']] = list(rand_activation
#                             ,rand_input_dropout
#                             ,rand_dropout
#                             #,TRUE
#                             ,rand_hidden
#                             ,2
#                             ,rand_l1
#                             ,rand_l2
#)
#names(models[[n]][['args']]) = c('activation'
#                                 ,'input_dropout_ratio'
#                                 ,'hidden_dropout_ratios'
#                                 #,'balance_classes'
#                                 ,'hidden'
#                                 ,'epochs'
#                                 ,'l1'
#                                 ,'l2'
#                                 
#)




return (models)

}

