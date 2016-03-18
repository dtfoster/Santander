  #load('postLoad2.RData')
  
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
  
  metric = 'my.auc'#'NormalizedGini'#'MultiLogLoss'#'perc_correct'
  
  doCV=TRUE
  doFull = TRUE
  
  
  
  models = getModels()
  num_models = length(models)
  
  modelsToChange = c(3)
  
  for (model_num in modelsToChange){
    
    cat('\n\n')
    time_stamp = format(Sys.time(), "%Y-%m-%d %H-%M-%S")
    models[[model_num]][['time_stamp']] = time_stamp
    models[[model_num]][['dataset']] = dataset
    
    m = models[[model_num]]
    name = m[['name']]
    
    rows = m[['numrows']]
    cols = m[['numcols']]
    if (rows==0) rows=nrow(X_train)
    if (cols==0) cols=ncol(X_train) 
    set.seed(m[['seed']])
    samp_rows = sort(sample(1:nrow(X_train),rows))
    samp_cols = sort(sample(1:ncol(X_train),cols))
  
    score = 0
    
    if (doCV){
      out = splitter(samp_rows,id_train,k,1)
      test_folds = out[['test']]
      train_folds = out[['train']]
      rm(out);gc()
      
      cat('Model',model_num, 'of',num_models,':',name)
      preds = array(NA, dim = c(length(y_train),length(classes),k))
      #setnames(preds,classes)
      fold_num = 0
      cat('\nFolds (',k,') :')
      for (fold in 1:k){
        #preds[,,k] = data.table(matrix(0,nrow = length(y_train),ncol = length(classes)))
        test_fold = test_folds[[fold]]
        train_fold = train_folds[[fold]]
        cat('\n',fold, ' \n')
        model = trainer(m,X_train[samp_rows,samp_cols,with=FALSE][train_fold],y_train[samp_rows][train_fold]) 
        gc()
        preds_fold = predictor(m,model,X_train[,samp_cols,with=FALSE][test_fold])
        score = evaluator(y_train[test_fold],preds_fold,metric)
        cat(' Score:',score)
        preds[test_fold,,fold]  = as.matrix(preds_fold)
        gc()
      } 
      
      preds = data.table(apply(preds,c(1,2),mean,na.rm=TRUE))
      setnames(preds,classes)
      
       names = paste(name,classes,sep='_')
     # blend_train[,names] = preds 
       cat('\nScoring the predictions...')
      score = evaluator(y_train,preds,metric)
      models[[model_num]][['score']] = score
      evaluations[model_num,c('model_id','model','score')] = c(model_num,name,score)
      cat('\n')
      cat('Score:',score)
      
      out = data.table(id_train,preds)
      setnames(out,c(id_col,classes))
      cat('\nWriting out the dataset to blend...')
      write.csv(out,file=paste0('./toBlend/',sprintf("%.6f", round(score,6)),' ',time_stamp,' ',name,'.csv'),row.names=FALSE)
      
      if (!doFull){
        train_score = 0
        write_scores(models[[model_num]])
      }
      
      
      
    }
    
    if (doFull){
  
      
      start_time = proc.time()[['elapsed']]
      cat('\n')
      cat('Training full model...')
      
      gc()
      #get.sizes()
      model = trainer(m,X_train[samp_rows,samp_cols,with=FALSE],y_train[samp_rows]) 
      cat('\n')
      end_time = proc.time()[['elapsed']]
      train_time = end_time - start_time
      cat(train_time,'s')
      cat('\n')
      model_bank[[model_num]] = model
      
    
      train_preds = predictor(m,model,X_train[,samp_cols,with=FALSE])
      setnames(train_preds,classes)
      cat('Scoring the predictions...')
      train_score = evaluator(y_train,train_preds,metric)
      models[[model_num]][['train_score']] = train_score
      evaluations[model_num,'train_score'] = train_score
      cat('\n')
      cat('Train Score:',train_score)
      cat('\n')
      
      
      cat('Predicting test set...')
      test_preds = predictor(m,model,X_test[,samp_cols,with=FALSE])
     # blend_test[,names] = test_preds
      
      out = data.table(id_test,test_preds)
      setnames(out,c(id_col,classes))
      path <- gzfile(paste0('./submissions/base/gz/',sprintf("%.6f", round(score,6)),' ',time_stamp,' ',name,'.csv.gz'))
      write.csv(out,file=path,row.names=FALSE)
      
      write.csv(out,file=paste0('./submissions/base/csv/',sprintf("%.6f", round(score,6)),' ',time_stamp,' ',name,'.csv'),row.names=FALSE)
      
      if (doCV){
        write_scores(models[[model_num]])
      }
  
      
    }
    
  
      
    
      
  }
