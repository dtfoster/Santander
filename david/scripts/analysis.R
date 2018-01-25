

tog = cbind(train,as.integer(y_train)-1)

variable = 'Comp 1'
var = tog[,.(mean(.SD[[variable]],na.rm=TRUE),sd(.SD[[variable]],na.rm=TRUE), length(.SD[[variable]])),by=V2]
var



var = var[order(var[[variable]])]
plot(var[[variable]],var$V1)
var

sapply(importance_matrix[['Feature']][1:10],function(x){str(train[[x]])})


xgb_model_num = 3

names <- colnames(X_train)
importance_matrix <- xgb.importance(names, model = model_bank[[xgb_model_num]])
xgb.plot.importance(importance_matrix)


duds = names(X_train)[which (!(names(X_train) %in% importance_matrix[['Feature']]))]


save(duds,file='duds1hot.RData')


rf_model_num = 1


varImpPlot(model_bank[[rf_model_num]])
plot(model_bank[[rf_model_num]])
legend("top",legend=c('all',classes),col=1:(1+length(classes)),cex=0.8,fill=1:(1+length(classes)))

# 
# for(i in 0:100){
#   
#   new_preds = data.frame((blend_train[,1] * i + (100-i) * blend_train[,2])/100)
#   cat(i,' : ', evaluator(y_train,new_preds,metric) , '\n' )
#   
# }



train_results = cbind(train_preds,y_train=y_train)
plot(train_results[order(train_results[,1]),2])




train_results[,'diff'] = (train_results[,3] - (train_results[,1]))
worst_stats = train_results[order(train_results[,'diff'],decreasing=TRUE),]
#train_results[order(train_results[,2],decreasing=TRUE),][1:10,]
worst_X = X_train[order(train_results[,'diff'],decreasing=TRUE),]


diff = train_results[,'diff']
diff_log = diff
diff_log[diff>0]<- log(diff[diff>0])
diff_log[diff<0]<- -log(-diff[diff<0])


  
  C_diff = cbind(worst_X,worst_stats)





id=which(round(X_train[,'T1_V8'],1)==7.8 )
new_X = X_train[id,]
new_y = train_results[id,'y_train']

set = 50
top_X = new_X[order(new_y,decreasing=TRUE)[c(1:set)],]
top_y = new_y[order(new_y,decreasing=TRUE)[c(1:set)]]
bottom_X = new_X[order(new_y,decreasing=TRUE)[c(((nrow(new_X)-(set-1)):nrow(new_X)))],]
bottom_y = new_y[order(new_y,decreasing=TRUE)[c(((nrow(new_X)-(set-1)):nrow(new_X)))]]

top_means = sapply(top_X,mean)

bottom_means = sapply(bottom_X,mean)

sort(top_means/bottom_means)







plot(y_train,sapply(blend_train[,1],function(x){max(x,0)}))

plot(y_train,blend_train[,1])
plot(y_train,sapply(blend_train[,1],function(x){max(x,0)}))

to_cluster = data.frame(x=sapply(train_results[,2],function(x){max(c(0,x))}),y=diff_log)

clusters = data.frame(to_cluster,cluster=kmeans(to_cluster,4)$cluster)
plot(clusters[,'x'],clusters[,'y'],col=clusters[,'cluster'])

for (i in 1:10){
  
  blend_train_new = blend_train * i
  
  print(RMSE(blend_train_new,y_train))
  
  
  
}




RF_top_dogs = submission[order(submission[,2],decreasing=TRUE),]
head(RF_top_dogs)
XGB_top_dogs = submission[order(submission[,2],decreasing=TRUE),]
head(XGB_top_dogs)
plot(RF_top_dogs[,2],XGB_top_dogs[,2])

abline(0,1,lwd=3,col=k)

top_dogs = merge(RF_top_dogs,XGB_top_dogs,by='Obs_ID')
colnames(top_dogs) = c('Obs_ID','RF','XGB')

plot(top_dogs[,2],top_dogs[,3])
abline(0,1,lwd=3,col=k)



submission[which(submission[,id_col]=='Obs_3673'),'Dependent'] = 1610000 # 480935.7 # 3.3268480589
submission[which(submission[,id_col]=='Obs_11724'),'Dependent'] = 950000 #364666.4 # 2.60512073501
submission[which(submission[,id_col]=='Obs_11251'),'Dependent'] = 625000 #302009.7 # 2.06946995411
submission[which(submission[,id_col]=='Obs_12270'),'Dependent'] = 580000 #246079.6 # 2.3722869485 968603
submission[which(submission[,id_col]=='Obs_715'),'Dependent'] = 580000 #241134.2


  submission[which(submission[,id_col]=='Obs_11519'),'Dependent'] = 30351 # 237119.2
submission[which(submission[,id_col]=='Obs_6725'),'Dependent'] = 144797 # 222418.53 17
submission[which(submission[,id_col]=='Obs_7808'),'Dependent'] = 22825  #  219251.4 18
submission[which(submission[,id_col]=='Obs_4838'),'Dependent'] =  272479 # 188490.0 19
submission[which(submission[,id_col]=='Obs_10788'),'Dependent'] = 83513 # 187162.8 20

new_train[blend_train[,1]>=240000] = 580000
new_train[blend_train[,1]>=300000] = 625000
new_train[blend_train[,1]>=360000] = 950000
new_train[blend_train[,1]>=480000] = 1610000



  top_dogs = submission[order(submission[,2],decreasing=TRUE),]
  
  
  
  
  
  #################################
  
  
  # Time to start looking at the features
  # Here is a visualization of the non-character variables
  

  library(corrplot)

  M <- X_train[,-c(2)]
  M <- cor(M)

  corrplot(M, method = "number",order = "alphabet",type='lower', diag=F, addCoefasPercent=T) 
  
  M <- X_test[,-c(2)]
  M <- cor(M)
  
  corrplot(M, method = "number",order = "alphabet",type='lower', diag=F, addCoefasPercent=T) 
  
  #############################
