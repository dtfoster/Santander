findLeaves = function(tree, currentnode){
  
  if (tree[currentnode,'Feature']=='Leaf'){
    leaves = currentnode
  }else{
    leftnode = tree[currentnode,Yes]
    rightnode = tree[currentnode,No]
    leaves = c(findLeaves(tree,'leftnode',with=FALSE),findLeaves(tree,'rightnode',with=FALSE))
  }
  
  return (sort(leaves))
  
  
}


findPath = function(tree, currentnode, path){

  while(currentnode>1){
    path = c(path,currentnode)
    currentlabel = tree[currentnode,ID]
    currentnode = c(which(tree[,Yes]==currentlabel),which(tree[,No]==currentlabel))
  }
  return (sort(c(path,1)))
  
}



################
getStatsForTrees = function(trees){
  
  dashPosition = sapply(trees$ID,function(x){regexpr("-",x) })
  trees[,tree:=as.integer(substring(ID,1,(dashPosition-1)))]
  trees[,node:=as.integer(substring(ID,dashPosition+1))]
  setkeyv(trees,c('tree','node'))   
  trees[Feature=='Leaf',G:=-Quality*Cover]
  trees[,H:=Cover]
  trees[Feature=='Leaf',weight:=Quality]
  trees[Feature=='Leaf',pred:=1/(1+exp(-weight))]
  trees[,previous_weight:=0]
  trees[,previous_pred:=0.5]
  trees[,uplift_weight:=0]
  trees[,uplift_pred:=0]
  
  trees = split(trees,as.factor(trees$tree))
  treenums =  as.character(0:(length(trees)-1))
  t = 0
  for (tree in trees){
    t=t+1
    rows = nrow(tree):1
    for (row in rows){
      if (tree[row,Feature]!='Leaf'){
        left = tree[row,Yes]
        right = tree[row,No]
        leftrow = which(tree[,ID]==left)
        rightrow = which(tree[,ID]==right)
        leftG = tree[leftrow,G]
        rightG = tree[rightrow,G]
        tree[row,'G':=leftG+rightG]
        tree[row,'weight':=-G/H]
        tree[row,'pred':=1/(1+exp(-weight))]
        
        w=tree[['weight']][row]
        
        tree[leftrow,'previous_weight':=w]
        tree[rightrow,'previous_weight':=w]
        tree[leftrow,'previous_pred':=1/(1+exp(-w))]
        tree[rightrow,'previous_pred':=1/(1+exp(-w))]
      
      }
    }
    cat(t,'\n')
    
    tree[,'uplift_weight':=weight-previous_weight]
    tree[,'uplift_pred':=pred-previous_pred]
  }
  
  return (trees)
}



################

getLeafStats = function(tree_list,emptyDF){
#    total_leaves = 0
#   for (tree in tree_list){
#    total_leaves =  total_leaves + sum(tree[['Feature']]=='Leaf')
#     
#   }
#   
#   
#   out = data.table(matrix(0.0,nrow=total_leaves,ncol=length(names(emptyDF))))
#   names(out) = names(emptyDF)
#   
  
  
  
  #out = list()
  cat('Getting statistics for each tree...\n')
  l=0
  out = emptyDF
  out[,c('tree','leaf','intercept'):=as.list(0,0,0)]
  out = out[-1,]
  
  for (x in 1:length(tree_list)){
    tree = tree_list[[x]]
    leaves = which(tree[['Feature']]=='Leaf')
    newlot=list()
    l=0
    for (leaf in leaves){
      l=l+1
      #start.time <- Sys.time()
      path = findPath(tree,leaf,c())
      #end.time <- Sys.time()
      #time.taken <- end.time - start.time
      #time.taken
      #start.time <- Sys.time()
      newlot[[l]]=getObsBreakdownForTree(tree,path,copy(emptyDF),x,leaf)
    

      #cat(l,'\n')
    }
    out = rbindlist(append(list(out),newlot))
    rm(newlot)
    gc()
    cat(x,'\n')
  }
  
  return (out)
  
}

getPredsBreakdown = function(leafstats,data_nodes){
  
  #out = rbindlist(out)
  

  cat('Extracting the breakdown of the probability for each client\n')
  
  for (col in 1:ncol(data_nodes)){
    column = data_nodes[,col] + 1
    reduced = leafstats[which(leafstats$tree==col),]
    
    out2 = reduced[match(column, reduced$leaf),]
    
    if (col==1){
      summary = out2[,3:ncol(out2),with=FALSE]
    }else{
      summary = summary + out2[,3:ncol(out2),with=FALSE]
    }
    cat(col,'\n')
  }
  #summary = summary / ntree
  #rownames(summary)=1:nrow(summary)
  return (summary)
  
}





getObsBreakdownForTree = function(tree,path,e,x,lf){

  reduced_tree = tree[path,.(Feature,uplift_weight)]
  e[,tree:=x]
  e[,leaf:=lf]
  e[,intercept:=reduced_tree[1,uplift_weight]]
  
 
  reduced_tree[,uplift_weight:=shift(uplift_weight,type='lead')]
  
  f = reduced_tree[,.(sum=sum(uplift_weight)),by=Feature]
  f = f[-nrow(f)]
  e[,(f[,Feature]):=as.list(f[,sum])]

  return (e)
}


