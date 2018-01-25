
library(readr)
library(data.table)
library(bit64)
library(caret)

cat("reading the train and test data\n")

train <- fread("./data/train.csv",integer64='numeric')
test <- fread("./data/test.csv",integer64='numeric')
submission <- fread("./data/sample_submission.csv",integer64='numeric')


id_col = 'ID'
response = 'TARGET'


y = train[,c(id_col,response),with=FALSE]
train[,response:=NULL,with=FALSE]



##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train[,n0:=apply(.SD, 1, FUN=count0)]
test[,n0:=apply(.SD, 1, FUN=count0)]

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[,(f):=NULL]
    test[,(f):=NULL]
  }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()

for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names,with=FALSE]

feature.names <- setdiff(names(test), toRemove)

test <- test[, feature.names,with=FALSE]





# 
# 
# # From manual data analysis
# datetimecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")
# 
# train_date_time_cols_raw <- train[,datetimecolumns,with=FALSE]
# train_date_num <- data.table(apply(train_date_time_cols_raw, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise
# train_date_time = sapply(train_date_time_cols_raw, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
# train_date_time = data.table(do.call(cbind.data.frame, train_date_time))
# 
# test_date_time_cols_raw <- test[,datetimecolumns,with=FALSE]
# test_date_num <- data.table(apply(test_date_time_cols_raw, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise
# test_date_time = sapply(test_date_time_cols_raw, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
# test_date_time = data.table(do.call(cbind.data.frame, test_date_time))
# 
# time_cols = c('VAR_0204','VAR_0217')
# train_time = train_date_time[,time_cols,with=FALSE]
# train_time = data.table(sapply(train_time, function(x) strftime(x, "%H:%M:%S")))
# train_hour = data.table(sapply(train_time, function(x) as.numeric(as.character(substr( x ,1, 2)))))
# setnames(train_hour,paste0(time_cols,'_hour'))
# test_time = test_date_time[,time_cols,with=FALSE]
# test_time = data.table(sapply(test_time, function(x) strftime(x, "%H:%M:%S")))
# test_hour = data.table(sapply(test_time, function(x) as.numeric(as.character(substr( x ,1, 2)))))
# setnames(test_hour,paste0(time_cols,'_hour'))
# 
# 
# train_diff_date = data.table(VAR_204_75_diff = as.integer(train_date_time[,VAR_0217] - train_date_time[,VAR_0075]))
# test_diff_date = data.table(VAR_204_75_diff = as.integer(test_date_time[,VAR_0217] - test_date_time[,VAR_0075]))

# 
# date_cols = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0169", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0217")
# train_date = train_date_time[,date_cols,with=FALSE]
# 
# train_year = data.table(sapply(train_date, function(x) as.integer(strftime(x, "%Y"))))
# train_month = data.table(sapply(train_date, function(x) as.integer(strftime(x, "%m"))))
# train_day = data.table(sapply(train_date, function(x) as.integer(strftime(x, "%d"))))
# setnames(train_year,paste0(date_cols,'_year'))
# setnames(train_month,paste0(date_cols,'_month'))
# setnames(train_day,paste0(date_cols,'_day'))
# test_date = test_date_time[,date_cols,with=FALSE]
# test_year = data.table(sapply(test_date, function(x) as.integer(strftime(x, "%Y"))))
# test_month = data.table(sapply(test_date, function(x) as.integer(strftime(x, "%m"))))
# test_day = data.table(sapply(test_date, function(x) as.integer(strftime(x, "%d"))))
# setnames(test_year,paste0(date_cols,'_year'))
# setnames(test_month,paste0(date_cols,'_month'))
# setnames(test_day,paste0(date_cols,'_day'))
# 
# 
# 
# 
# for (dc in datetimecolumns){
#   train[,dc:=NULL,with=FALSE]
#   test[,dc:=NULL,with=FALSE]
# }
# 
# train = cbind(train,train_date_num,train_hour,train_diff_date,train_year,train_month,train_day)
# test = cbind(test,test_date_num,test_hour,test_diff_date,test_year,test_month,test_day)
# 



library(dplyr)
library(stringdist)

# 
# 
# cat("Nr of city names before cleanup:", length(unique(train$VAR_0200)), fill=T)
# 
# reviewDupes <- mutate(train, City = VAR_0200, State = VAR_0237, Zip=VAR_0241) %>% 
#   select(City, State, Zip) %>%
#   mutate(stateZip = paste(Zip, State, sep="_"),
#          fullGeoID = paste(City, Zip, State, sep="_")) %>%
#   distinct()
# potentialDupes <- group_by(reviewDupes, stateZip) %>% 
#   dplyr::summarise(n = n(), 
#                    altName = first(City), # prettier: most common
#                    altID = first(fullGeoID)) %>% 
#   filter(n > 1)
# dupes <- mutate(left_join(potentialDupes, reviewDupes, by="stateZip"), 
#                 dist=stringdist(altName, City)) %>% 
#   filter(dist >= 1 & dist <= 2)
# 
# write_csv(select(dupes, City, State, Zip, altName), "CleanedupCities.csv")
# 
# print("Preview:")
# print(head(paste(dupes$City, dupes$State, "=>", dupes$altName), 20))
# 
# train <- mutate(train, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
# train <- left_join(train, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
#   mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
#   select(-fullGeoID, -altName)
# # and do the same for the test set
# 
# test <- mutate(test, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
# test <- left_join(test, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
#   mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
#   select(-fullGeoID, -altName)
# 
# cat("Nr of city names after cleansing:", length(unique(train$VAR_0200)), fill=T)
# 
# train = data.table(train)
# test = data.table(test)
# 
# gc()

feature.names <- names(train)
feature.names <- feature.names[feature.names!=response]
# names(train)  # 1934 variables


char_feat = c()
for (f in feature.names) {
  if (class(train[[f]]) %in% c("character","logical")) {
    char_feat = c(char_feat, f)
  }
}

save(char_feat,file='RData/char_feat.RData')

sapply(char_feat,function(x){length(unique(train[[x]]))})



cat("Numerical column count : ", dim(train[, .SD, .SDcols = names(sapply(train, is.numeric))[sapply(train, is.numeric)]])[2], 
    "; Character column count : ", dim(train[, .SD, .SDcols = names(sapply(train, is.character))[sapply(train, is.character)]])[2])




cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in char_feat) {
    num_levels= nrow(unique(train[,f,with=FALSE]))
    if (num_levels>10){
      levels = train[,c(f,response),with=FALSE]
      #levels = level[,.(countcol = length(get(response)),avg = mean(as.integer(get(response))),sumcol = sum(as.integer(get(response)))),by=f]
      levels = levels[,.(countcol = length(get(response))),by=f]
      
      levels[,rank:=rank(-countcol,ties.method="random")]
      
      setkeyv(train,f)
      setkeyv(test,f)
      setkeyv(levels,f)
      
      #train[,(f):=((levels[.SD])[,sumcol] - .SD[[response]]) / ((levels[.SD])[,countcol]-rep(1,nrow(.SD)))]
      #test[,(f):=(levels[.SD])[,avg]]
      
      train[,(f):=(levels[.SD])[,rank]]
      test[,(f):=(levels[.SD])[,rank]]
      
      gc()
    }else{

      
      train_col = train[,as.factor(f),with=FALSE]
      test_col = test[,as.factor(f),with=FALSE]
      
      dummy = dummyVars( ~ ., data = train_col)
      
      test_out = data.table(predict(dummy, newdata = test_col))
      train_out = data.table(predict(dummy, newdata = train_col))[,names(test_out),with=FALSE]
      
      train = cbind(train,train_out)
      test = cbind(test,test_out)
      
      rm(train_col,test_col,dummy,train_out,test_out)
      gc()
      
      train[,f:=NULL,with=FALSE]
      test[,f:=NULL,with=FALSE]

    }
    
    cat('\n',f)
  
}

# 
# for (row in which(train[,VAR_0212]=='9218868437227407266')){
#   set(train,i=row,j=which(names(train)=='VAR_0212'),0)
#   
# }
# 
# for (row in which(test[,VAR_0212]=='9218868437227407266')){
#   set(test,i=row,j=which(names(test)=='VAR_0212'),0)
# }


# 
# train[,VAR_0212:=as.integer(VAR_0212/1000)]
# test[,VAR_0212:=as.integer(VAR_0212/1000)]
# 

cat("replacing missing values with -1\n")

for (j in seq_len(ncol(train)))
  set(train,which(is.na(train[[j]])),j,-1)

for (j in seq_len(ncol(test)))
  set(test,which(is.na(test[[j]])),j,-1)


#for (j in seq_len(ncol(train)))
#  set(train,which(train[[j]]>1e+8),j,-1)

#for (j in seq_len(ncol(test)))
#  set(test,which(test[[j]]>1e+8),j,-1)





train.unique.count=lapply(train, function(x) length(unique(x)))
train.unique.count_1=unlist(train.unique.count[unlist(train.unique.count)==1])
train.unique.count_2=unlist(train.unique.count[unlist(train.unique.count)==2])

#train2 = train[,.SD,.SDcols = names(train.unique.count_2)]

#nrow(train2) - nrow(unique(train2))


delete_const=names(train.unique.count_1)
# delete_NA56=names(which(unlist(lapply(train[,names(train.unique.count_2),with=FALSE], function(x) max(table(x,useNA='always'))))==145175))
# delete_NA89=names(which(unlist(lapply(train[,names(train.unique.count_2),with=FALSE], function(x) max(table(x,useNA='always'))))==145142))
# delete_NA918=names(which(unlist(lapply(train[,names(train.unique.count_2),with=FALSE], function(x) max(table(x,useNA='always'))))==144313))



#VARS to delete
#safe to remove VARS with 56, 89 and 918 NA's as they are covered by other VARS
print(length(c(delete_const)))#,delete_NA56,delete_NA89,delete_NA918)))

if (length(delete_const)>0) {
train[,c(delete_const):=NULL,with=FALSE]
test[,c(delete_const):=NULL,with=FALSE]
}



cat("Numerical column count : ", dim(train[, .SD, .SDcols = names(sapply(train, is.numeric))[sapply(train, is.numeric)]])[2], 
    "; Character column count : ", dim(train[, .SD, .SDcols = names(sapply(train, is.character))[sapply(train, is.character)]])[2])


#write.csv(train,file = 'train_tab.csv')






setkeyv(train,id_col)
setkeyv(test,id_col)






write_csv(train,path='dataProcessed/X_train_1hot.csv')
write_csv(test,path='dataProcessed/X_test_1hot.csv')
write_csv(y,path='dataProcessed/y_train.csv')
write_csv(submission,path='dataProcessed/y_test.csv')







