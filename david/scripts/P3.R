# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 
# PREDICTING PRODUCT PURCHASE
# 
# 18/01/16
# 
# GENERATE FILES FOR PRODUCT PURCHASE PREDICTOR TOOL
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cat("\n\nLOADING...")

start_time = proc.time()[['elapsed']]
cat('\n')

# Load libraries
cat("\nLoading libraries....")


library(data.table)
library(xgboost)
library(readr)
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

trees = xgb.model.dt.tree(names(X_train), model = model)
tree_list = getStatsForTrees(trees)
emptyDF = data.table(matrix(0,ncol=ncol(X_train)+3,nrow=1))
names(emptyDF) = c('tree','leaf','intercept',names(X_train))
leafstats = getLeafStats(tree_list,emptyDF)

save(leafstats,file='./analysis/leafstats.rdata')
#save(trees,tree_list,file='./analysis/trees.rdata')

#rm(trees,tree_list)

gc()

# EDIT THIS LINE TO CHANGE THE ROWS INCLUDED IN THE TOOL
rows = c(1:1000)

for (i in list(rows)){

#for (i in list(c(1:100000),c(100001:200000),c(200001:300000),c(300001:400000),c(400001:500000),c(500001:600000),c(600001:700000),c(700001:nrow(X_test)))){
  
  cat(i[1],'\n')
  pred = predict(model,xgb.DMatrix(data.matrix(X_test[i])))
  cat('Prediction Complete','\n')
  nodes = predict(model,xgb.DMatrix(data.matrix(X_test[i])),predleaf =TRUE)
  cat('Nodes Complete','\n')
  
  pred.breakdown = getPredsBreakdown (leafstats,nodes)
  cat('Breakdown Complete','\n')
  weights = rowSums(pred.breakdown)
  pred.xgb = 1/(1+exp(-weights))
  cat(max(pred-pred.xgb),'\n')
  if (i[1]==1){
    pred.breakdown.master = pred.breakdown
  }else{
    pred.breakdown.master = rbindlist(list(pred.breakdown.master, pred.breakdown))
  }
  cat('Binding Complete','\n')
  rm(nodes)
  rm(pred.breakdown)
  rm(pred)
  gc()
  
}

#rm(leafstats)
gc()

weights = rowSums(pred.breakdown.master)
pred.xgb = 1/(1+exp(-weights))


pred.breakdown.master[,pred:=pred.xgb]
pred.breakdown.master[,record_ID:=id_test[rows]]
pred.breakdown.master[,weights:=weights]

save(pred.breakdown.master,file='./analysis/contributions.rdata')
write.csv(pred.breakdown.master,file='./analysis/data.csv',row.names=FALSE)
write.csv(data.table(record_ID=id_test[rows],X_test[rows]),file='./analysis/variable_data.csv',row.names=FALSE)

end_time = proc.time()[['elapsed']]
train_time = end_time - start_time
cat(train_time,'s\n')

head(pred.breakdown.master)

sort(colMeans(pred.breakdown.master))
