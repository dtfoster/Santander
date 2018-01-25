predictor=function(m,model,X){
  
  total_preds <- NULL
  
  batchsize = round(min(10000,nrow(X)/10))
  complete = 0
  
 
  cat ('Predicting...\n')
  
  for (rows in split(1:nrow(X), ceiling((1:nrow(X))/batchsize))) {
    
    data = XSetter(m,X[rows])
    
    if (m[['model_type']] %in% c('randomForest','lm')){
      if (response_type=='continuous'){
        preds= predict(model,data[['x']])
      }else if (response_type=='discrete'){
        preds= predict(model,data[['x']],type="prob")
      }else if (response_type=='binary'){
        if (metric == 'perc_correct'){
        preds= as.integer(predict(model,data[['x']],type="response"))-1
        }else{
          preds= predict(model,data[['x']],type="prob")[,'1']  
        }
      }
      
    }
  
    
    if (m[['model_type']] %in% c('h2o.deeplearning')){
      if (response_type=='continuous'){
        preds= as.data.table(predict(model,data[['newdata']]))
      }else if (response_type=='discrete'){
        preds= as.data.frame(predict(model,data[['newdata']],type="prob"))[,classes]
      }else if (response_type=='binary'){
        if (metric == 'perc_correct'){
        preds= as.data.table(predict(model,data[['newdata']]))[['predict']]
        }else{
          preds= as.data.table(predict(model,data[['newdata']]))[['p1']] 
          
        }
      }
      
    }
    
    if (m[['model_type']] %in% c('cv.glmnet')){
      if (response_type=='continuous'){
        preds= predict(model,data[['x']],type = 'response',s="lambda.min")
      }else if (response_type=='discrete'){
        preds= predict(model,data[['x']],type="response",s="lambda.min")[,,1]
        
      }else if (response_type=='binary'){
        if (metric == 'perc_correct'){
        preds= as.integer(predict(model,data[['x']],type="class",s="lambda.min"))
        }else{
          preds= predict(model,data[['x']],type="response",s="lambda.min")[,1]  
          
        }
      }
      
    }
    
  
    
    if (m[['model_type']] == 'xgb.train'){
      if (response_type=='continuous'){
        preds= predict(model,data[['data']])
      }else if (response_type=='discrete'){
        num_class = m[['args']][['param']][['num_class']]
        preds= predict(model,data[['data']])
        preds = matrix(preds,nrow = num_class)
        preds = t(preds)
      }else if (response_type =='binary'){
        if (metric == 'perc_correct'){
          preds= predict(model,data[['data']])
            preds = round(preds)
        }else{
          preds= predict(model,data[['data']])
          
        }
        #preds = matrix(preds,ncol = 2,byrow=TRUE)
        #preds = preds[,2]
        
      }
      
      
    }
    
    if (m[['model_type']] == 'multinom'){
      preds= predict(model,data[['x']],type="probs")
    }
    
    if (is.null(total_preds)){

      total_preds = data.table(preds)
      names(total_preds) = classes
      }else{
        #print(head(preds))
        if (is.vector(preds)){
          preds = data.frame(matrix(preds,ncol=length(classes))) 
        }else{
        preds = data.frame(as.matrix(preds,ncol=length(classes)))
        }
        colnames(preds) = classes
        #print(head(total_preds))
        #print(head(preds))
      total_preds = rbind(total_preds,preds)
    }
    
   # print(total_preds)
  #  print(preds)
  #  print(data.table(preds))
    
    complete = complete + length(rows)
    cat (' ', round(complete/nrow(X) * 100))
    gc()
  
  }
  
  setnames(total_preds,classes)
  

  
  cat ('\n')
  return(total_preds)
  
}