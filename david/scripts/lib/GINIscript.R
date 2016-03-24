load('postLoadPP.RData')
load('importance_matrix_2.RData')

best.vars = importance_matrix[1:100,Feature]

combos = data.table(var1 = character(),var2 = character(),method = character(), err = numeric(), errNEW = numeric(), errdiff=numeric())


for (i in 1:99){
  var1 = best.vars[i]
  for (j in (i+1):100){
    var2 = best.vars[j]
    
    out =  getGINI(X_train[,var1,with=FALSE],X_train[,var2,with=FALSE],y_train)
    combos = rbind(combos,out)
    cat(i, ' ',j, '\n')
    print(out)
  }
}

combos = combos[which(combos[,errdiff< -0.001])]
combos = combos[order(errdiff)]


for (row in 1:nrow(combos)){
  type = substring(combos[row,method],4,nchar(combos[row,method]))
  var1 = combos[row,var1]
  var2 = combos[row,var2]
  newvar = paste0(var1,'_',type,'_',var2)
  if (type == 'add'){
    X_train[,(newvar):=.SD[,var1,with=FALSE] + .SD[,var2,with=FALSE]]
    X_test[,(newvar):=.SD[,var1,with=FALSE] + .SD[,var2,with=FALSE]]
  }else if (type == 'sub'){
    X_train[,(newvar):=.SD[,var1,with=FALSE] - .SD[,var2,with=FALSE]]
    X_test[,(newvar):=.SD[,var1,with=FALSE] - .SD[,var2,with=FALSE]]    
    
  }else if (type == 'mult'){
    
    X_train[,(newvar):=.SD[,var1,with=FALSE] * .SD[,var2,with=FALSE]]
    X_test[,(newvar):=.SD[,var1,with=FALSE] * .SD[,var2,with=FALSE]]
    
  }else if (type == 'div'){
    
    X_train[,(newvar):=.SD[,var1,with=FALSE] / (.SD[,var2,with=FALSE]+912.34567)]
    X_test[,(newvar):=.SD[,var1,with=FALSE] / (.SD[,var2,with=FALSE]+912.34567)]
    
  }
  
 cat(row,'\n')
  
}