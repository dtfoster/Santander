splitter = function(samp_rows,id_train,k,type){
  #type = -1 if train smaller than test
  #type = 1 if train bigger than test

  library(caret)
  
  n = length(samp_rows)  
  p = length(id_train) 
  indices = createFolds(1:n,k) # positions based on development
  
  if (type == 1){
    
   indices = lapply(indices,function(x) which(!(c(1:n) %in% x)))
    
  }
  
  ids_in_folds = lapply(indices,function(x) id_train[samp_rows][x]) # IDS based on development
  out = lapply(ids_in_folds,function(x) which(id_train %in% x)) #positions based on full
  

    test_folds = lapply(out,function(x) c(1:p)[-x])
    train_folds = lapply(indices,function(x) c(1:n)[x])

  
  return (list(train=train_folds,test=test_folds))
  
  
}