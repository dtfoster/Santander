# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 
# PREDICTING PRODUCT PURCHASE
# 
# 18/01/16
# 
# ANALYSE
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n\nLOADING...")

start_time = proc.time()[['elapsed']]
cat('\n')

# Load libraries
cat("\nLoading libraries....")

rm(list = ls())
gc()
library(data.table)
library(xgboost)
library(readr)
library(corrplot)
source('./scripts/lib/xgbMining.R')

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
y_train[,c(id_col) := NULL]

# Load the model
cat("\nLoading the model...")
load('./models/model.rdata')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(corrplot)

idx = which(sapply(X_train,class) == 'numeric')

M <- cor(X_train[,idx,with=FALSE])

corrplot(M, method = "number"
         ,order = "hclust",type='lower', diag=F
         , addCoefasPercent=T
) 

save(M,file='./analysis/correlation.rdata')
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

importance_matrix <- xgb.importance(names(X_test), model = model)
importance_matrix[1:50]
save(importance_matrix,file='./analysis/importance_matrix.rdata')


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\nFINISHED!")
