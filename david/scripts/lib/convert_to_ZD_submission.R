

filenames_train <- list.files("./toBlend/train", pattern="*.csv", full.names=FALSE)
filenames_test <- list.files("./toBlend/test", pattern="*.csv", full.names=FALSE)

for (file in filenames_train){
  
  AUC = substring(file,1,8)
  dt = substring(file,10,28)
  dt = as.numeric(as.POSIXct(dt, format = "%Y-%m-%d %H-%M-%S"))
  newName = paste('d',AUC,dt,'train.csv',sep='_')
  file.rename(paste0('./toBlend/train/',file),paste0('./toBlend/train/',newName,'.csv'))
  
  
}


for (file in filenames_test){
  
  AUC = substring(file,1,8)
  dt = substring(file,10,28)
  dt = as.numeric(as.POSIXct(dt, format = "%Y-%m-%d %H-%M-%S"))
  newName = paste('d',AUC,dt,'test.csv',sep='_')
  file.rename(paste0('./toBlend/test/',file),paste0('./toBlend/test/',newName,'.csv'))
  
  
}
