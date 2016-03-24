importance_matrix <- xgb.importance(names(X_train), model = model_bank[[3]])
save(importance_matrix,file='importance_matrix.RData')
xgb.plot.importance(importance_matrix)
importance_matrix

duds = names(X_train)[which (!(names(X_train) %in% importance_matrix[['Feature']]))]
duds
save(duds,file='duds.RData')

lapply(X_train[,duds,with=FALSE],table)

tester = cbind(X_train,as.integer(y_train)-1)
