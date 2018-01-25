rm(list = ls())
gc()
load('postLoad.RData')

id_train =  data.frame(id_train )
colnames(id_train) = id_col

id_test =  data.frame(id_test )
colnames(id_test) = id_col


library(caret)

plsdaModel=plsda(X_train,y_train,center=TRUE,scale=TRUE,ncomp=100,probMethod='softmax') #Bayes
loadings = loadings(plsdaModel)[,]
rm(plsdaModel)
gc()


PP = preProcess(X_train,method = c("center", "scale"))


new_X_train = cbind(id_train,data.table(as.matrix(predict(PP,X_train)) %*% loadings))
write_csv(new_X_train,path='dataProcessed/X_train_pls.csv')

rm(new_X_train)
rm(X_train)
gc()



new_X_test = cbind(id_test,data.table(as.matrix(predict(PP,X_test)) %*% loadings))
write_csv(new_X_test,path='dataProcessed/X_test_pls.csv')



