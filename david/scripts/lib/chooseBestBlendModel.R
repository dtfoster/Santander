chooseBestBlendModel = function(blend_evaluations,direction){
  bestScore = -direction*9999999999999
  for (row in 1:nrow(blend_evaluations)){
    row = blend_evaluations[row,]
    score = as.numeric(row[,'score'])
    if (!is.na(score)){
      if ((-direction*(score-bestScore))<0){
        bestScore = score
        bestModel = as.integer(row[,'model_id'])
      }
    }
    
    
  }
  
  return (bestModel)
  
  
}