response_idx =  which(names(train)==response)
ID_idx =  which(names(train)==id_col)


newtrain=train[]
newtest = test[]

t = 0
for (i in names(newtrain)[-c(ID_idx,response_idx)]){
  t = t+1
  PP = preProcess(newtrain[,i,with=FALSE], method = c("center","scale"))
  newtrain[,c(i):=predict(PP,.SD[,i,with=FALSE])]
  newtest[,c(i):=predict(PP,.SD[,i,with=FALSE])]
  gc()
  cat(t,' ',i,'\n')
}




write_csv(newtrain[,(response):=NULL],path='dataProcessed/X_train_1hot_PP2.csv')
write_csv(newtest,path='dataProcessed/X_test_1hot_PP2.csv')


