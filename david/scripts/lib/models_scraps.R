
# 
# 
# #for ( max_depth in c(3,7)){
# #  for ( eta in c(0.002,0.6)){
# #    for ( min_child_weight in c(1,10,100)){
# max_depth = 4
# eta = 0.2
# #min_child_weight = 100
# 
# 
# n = n + 1
# 
# models[[n]] = list()
# models[[n]][['num']] = n
# models[[n]][['name']] = paste0('XGB',n)
# models[[n]][['model_type']] = 'xgb.train'
# models[[n]][['args']] = list(list('max_depth'=max_depth
#                                   , 'eta'=eta
#                                   , 'silent'=1
#                                   #, 'gamma'= 0
#                                   , 'objective'='multi:softprob'
#                                   , 'num_class'=3
#                                   ,'min_child_weight'= min_child_weight
#                                   ,'subsample'= 1
#                                   #,'max_delta_step'= 1
#                                   ,'colsample_bytree'= 0.8
#                                   #,"scale_pos_weight" = 1.0
#                                   #,'eval_metric'= 'rmse'
#                                   #,'lambda' = 1 
# ),2000,1,FALSE,10)
# names(models[[n]][['args']]) = c('param','nrounds','verbose','maximize','earlyStopRound')
# 
#  #   }
# #  }
# #}
# 
# 
# 
# 
# n = n+1
# 
# models[[n]] = list()
# models[[n]][['num']] = n
# models[[n]][['name']] = 'RF2'
# models[[n]][['model_type']] = 'randomForest'
# models[[n]][['args']] = list(100
#                              ,0
#                              ,10000
#                              ,5
# )
# names(models[[n]][['args']]) = c('ntree'
#                                  ,'verbose'
#                                  ,'sampsize'
#                                  ,'nodesize'
#                                  #,mtry
#                                  #,nodesize
# )
# 
