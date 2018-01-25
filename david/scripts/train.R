# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 
# PREDICTING PRODUCT PURCHASE
# 
# 18/01/16
# 
# TRAIN
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n\nLOADING...")

# Load libraries
cat("\nLoading libraries....")

rm(list = ls())
gc()
library(data.table)
library(xgboost)
library(readr)

# Load the data produced from the preProcess.R script
cat("\nLoading the data....")
id_col = 'ID'
response_col = 'TARGET'

cat("\n...train....")
X_train = fread('./dataProcessed/X_train_1hot.csv')
id_train = X_train[[id_col]]
setkeyv(X_train,id_col)
X_train[,c(id_col) := NULL]

cat("\n...test....")
X_test = fread('./dataProcessed/X_test_1hot.csv')
id_test = X_test[[id_col]]
setkeyv(X_test,id_col)
X_test[,c(id_col) := NULL]

cat("\n...response....")
y_train = fread('./dataProcessed/y_train.csv')
setkeyv(y_train,id_col)
y_train = y_train[,get(response_col)]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n\nTRAINING...")

# Train the model
cat("\nSetting the parameters for the model....")
param = list(
  eta = 0.01
  , gamma = 0
  , max_depth = 8
  , min_child_weight = 1
  , subsample = 0.7
  , colsample_bytree = 0.8
  , num_parallel_tree = 1
  ,objective = 'binary:logistic'
  )

cat("\nBuilding the data matrix for the xgboost model....")
data = xgb.DMatrix(data.matrix(X_train), label=y_train)

cat("\nSetting the arguments for the model....")
args = list(
  param = param
  , verbose = 1
  , maximize=TRUE
  , nrounds = 5880
  , print.every.n=1
  , data = data 
  , watchlist = list(train = data)
  , eval_metric = 'auc'
  )


start_time = proc.time()[['elapsed']]
cat('\n')

cat('Training the model...')
set.seed(3)
model = do.call('xgb.train',args)

end_time = proc.time()[['elapsed']]
train_time = end_time - start_time
cat(train_time,'s\n')

cat('Saving the model...')
save(model,file='./models/model.rdata')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n\nPREDICTING...")

# Predicting the test set
cat("\nPredicting the test set...")
preds = predict(model,xgb.DMatrix(data.matrix(X_test)))
threshold=0.49
preds[which(preds>1-threshold)]=1
preds[which(preds<=1-threshold)]=0
preds = data.table(preds)
setnames(preds,response_col)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n\nWRITING...")

# Writing the prediction to csv
cat("\nWriting the prediction to csv...")
write_csv(preds,path=paste0('./submissions/submission.csv'))    

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n\nFINISHED...")


