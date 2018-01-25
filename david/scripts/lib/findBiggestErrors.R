findBiggestErrors = function(train_preds,y_train,X_train,id_train
                             ){
  err = y_train[[response_col]] - train_preds[[response_col]]
  abs_err = abs(err)
  
  dt = cbind(id_train,y_train,train_preds,err,abs_err)
  names(dt) = c(id_col,'act','preds','err','abs_err')
  
  dt = dt[order(abs_err,decreasing=TRUE)]
  
  head(dt[,])
  head(X_train[order(abs_err,decreasing=TRUE),])
  
}