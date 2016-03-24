#load('postLoad2.RData')

set.seed(1066)
setwd("C:/Users/david.foster/Desktop/GitHub/Data Science/Springleaf")

library(caret)
library(data.table)
source('./scripts/lib/functions.R')
source('./scripts/lib/loader.R')
source('./scripts/lib/splitter.R')
source('./scripts/lib/trainer.R')
source('./scripts/lib/argSetter.R')
source('./scripts/lib/predictor.R')
source('./scripts/lib/metrics.R')
source('./scripts/lib/evaluator.R')
source('./scripts/lib/blendModels.R')
source('./scripts/lib/chooseBestBlendModel.R')

id_col = 'ID'
response_col = 'target'
response_type = 'binary'
metric = 'my.auc'
toRemove = c()

l=5

filenames_train <- list.files("./toBlend", pattern="*.csv", full.names=FALSE)
filenames_test <- list.files("./submissions/base/csv", pattern="*.csv", full.names=FALSE)

filenames = intersect(filenames_train,filenames_test)


id_train = loader_X(paste0('./toBlend/',filenames[1]),id_col,toRemove)[[1]]
id_test = loader_X(paste0('./submissions/base/csv/',filenames[1]),id_col,toRemove)[[1]]


blend_train = data.table(id_train)
colnames(blend_train) = id_col
blend_test = data.table(id_test)
colnames(blend_test) = id_col

for (file in filenames){
  
  d = loader_X(paste0('./toBlend/',file),id_col,toRemove)[[2]]
  setnames(d,file) 
  if (nrow(d) == length(id_train)){
 
    blend_train = cbind(blend_train,d)
  }
  
  
}




#blend_train = data.table(cbind(blend_train,out))

for (file in filenames){
  
  d = loader_X(paste0('./submissions/base/csv/',file),id_col,toRemove)[[2]]
  setnames(d,file) 
  if (file %in% names(blend_train)){
    blend_test = cbind(blend_test,d)
  }
  
  
}



blend_train[,(id_col):=NULL]
blend_test[,(id_col):=NULL]



#PP = preProcess(blend_train, method = c("center","scale","YeoJohnson"))
#blend_train = data.table(predict(PP,blend_train))
#blend_test = data.table(predict(PP,blend_test))



blend_train = cbind(X_train,blend_train)
blend_test = cbind(X_test,blend_test)




####START COPY FROM HERE


blend_models = getBlendModels()
num_blend_models = length(blend_models)

samp_rows = 1:nrow(blend_train)
out = splitter(samp_rows,id_train,l,1)
test_folds = out[['test']]
train_folds = out[['train']]

blend_evaluations = data.frame(model_id=numeric(),model = character(),score=numeric(),train_score=numeric(),stringsAsFactors =FALSE)
blend_model_bank = list()


blend_modelsToChange = c(3)

for (model_num in blend_modelsToChange){
  cat('\n\n')
  time_stamp = format(Sys.time(), "%Y-%m-%d %H-%M-%S")
  blend_models[[model_num]][['time_stamp']] = time_stamp
  blend_models[[model_num]][['dataset']] = 'blend'
  
  m = blend_models[[model_num]]
  name = m[['name']]
  
  cat('Blend Model',model_num, 'of',num_blend_models,':',name)
  preds = data.frame(matrix(NA,nrow = length(y_train),ncol = length(classes)))
  colnames(preds) = classes
  fold_num = 0
  cat('\nFolds (',l,') :\n')
  for (fold in 1:l){
    test_fold = test_folds[[fold]]
    train_fold = train_folds[[fold]]
    
    cat(fold, ' \n')

    
    model = trainer(m,blend_train[train_fold],y_train[train_fold]) 
    preds[test_fold,]  = predictor(m,model,blend_train[test_fold]) 
    gc()
  } 
  
  score = evaluator(y_train,preds,metric)   
  blend_models[[model_num]][['score']] = score
  blend_evaluations[model_num,c('model_id','model','score')] = c(model_num,name,score)
  cat('\n')
  cat('Score:',score) 
  
}

model_num = chooseBestBlendModel(blend_evaluations,1)
m = blend_models[[model_num]]
blendModel = trainer(m,blend_train,y_train) 

cat('Predicting training set...')
train_preds = data.frame(predictor(m,blendModel,blend_train))
train_score = evaluator(y_train,train_preds,metric)
blend_models[[model_num]][['train_score']] = train_score
blend_evaluations[model_num,'train_score'] = train_score
cat('\n')
cat('Train Score:',train_score)
cat('\n')

write_scores(blend_models[[model_num]])

cat('Predicting test set...')
#submission = blend_test[,2]

#submission[submission<0]=0
test_preds  = predictor(m,blendModel,blend_test) 

out = data.table(id_test,test_preds)
setnames(out,c(id_col,classes))
path <- gzfile(paste0('./submissions/blend/',sprintf("%.6f", round(score,6)),' ',time_stamp,' ',name,'.csv.gz'))
write.csv(out,file=path,row.names=FALSE)


#write.csv(submission,file=paste0('./submissions/blend/',sprintf("%.6f", round(score,6)),' ',time_stamp,' ',m[['name']],'.csv'),row.names=FALSE)

#blendModel$glmnet.fit$beta


 
 names <- colnames(blend_train)
 importance_matrix <- xgb.importance(names, model = blendModel)
 xgb.plot.importance(importance_matrix)


