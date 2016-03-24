loader_X = function(path,id_col,toRemove){
  

  X = fread(path,integer64='numeric')

  id = NULL
  
  if (!(missing(id_col))){

    id = X[[id_col]]
    setkeyv(X,id_col)
    X[,c(id_col) := NULL]
  }
  
  
  if (!(is.null(toRemove))){
    X <- X[,toRemove:=NULL,with=FALSE]

  }
  
  
  return (list(id,X))
  
}

loader_y = function(path,id_col,response_col){
  

  y = fread(path,integer64='numeric')

  
  if (!(missing(response_col))){
    id = y[[id_col]]
    setkeyv(y,id_col)
    y[,c(id_col) := NULL]
    y = y[[response_col]]
  }
  
  
  
  return (list(y))
  
}



loader = function(path,id_col,response_col,toRemove){
  

  data = fread(path,integer64='numeric')

  id = NULL
  X = data
  y = NULL
  
  if (!(missing(id_col))){

    id = X[[id_col]]
    setkeyv(X,id_col)
    X[,id_col := NULL]

  }
  
  
  if (!(missing(response_col))){

    y = X[[response_col]]
    X[,response_col:=NULL]
  }
  
  if (!(is.null(toRemove))){
    X <- X[,toRemove:=NULL,with=FALSE]

  }
  
  
  return (list(id,X,y))
  
}