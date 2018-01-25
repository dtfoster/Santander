
set.seed(0)


library(data.table)
library(bit64)

library(readr)
source('./scripts/lib/functions.R')
source('./scripts/lib/loader.R')
source('./scripts/lib/splitter.R')
source('./scripts/lib/trainer.R')
source('./scripts/lib/argSetter.R')
source('./scripts/lib/predictor.R')
source('./scripts/lib/metrics.R')
source('./scripts/lib/evaluator.R')
source('./scripts/lib/models.R')
id_col = 'ID'
response_col = 'TARGET'

response_type = 'binary'
binary_type = 'prob'

k =5


safety.wheels = FALSE

#load('duds1hot.RData')
toRemove = c()

dataset = '1hot'

############################################

out = loader_X(paste0('./dataProcessed/X_train_',dataset,'.csv'),id_col,toRemove)
id_train =  out[[1]] 
X_train = out[[2]]
rm(out); gc()

y_train = loader_y('./dataProcessed/y_train.csv',id_col,response_col)[[1]]

if (response_type=='discrete'){
  y_train = as.factor(y_train)
  classes = levels(y_train)
  num_classes = length(classes)
}else if (response_type == 'continuous'){
  classes = response_col
}else if (response_type == 'binary'){
  y_train = as.factor(y_train)
  classes = response_col
 # num_classes = 2
}



if (safety.wheels == TRUE){
  h = sample(1:nrow(X_train),1000)
  X_train = X_train[h]
  id_train = id_train[h]
  y_train = y_train[h]
}


out = loader_X(paste0('./dataProcessed/X_test_',dataset,'.csv'),id_col,toRemove)
id_test =  out[[1]] 
X_test = out[[2]]

rm(out)
gc()


evaluations = data.frame(model_id=numeric(),model = character(),score=numeric(),train_score=numeric(),stringsAsFactors =FALSE)
model_bank = list()












