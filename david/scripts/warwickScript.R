

library(Hmisc)
library(caret)
library(reshape2)
library(lattice)
library(psych)
library(vegan)
library(rela)
library(MASS)
library(klaR)
library(qualityTools)
source("BoxM.R")
library(DiscriMiner)
library(ggplot2)
library(mvnmle)
library(BaylorEdPsych)


data = X_train #read.csv('data.csv',stringsAsFactors=FALSE)

sort(sapply(data, function(x) {round(100*sum(is.na(x))/length(x),1)}))

binary = sapply(data[3:ncol(data)], function(x){as.integer(is.na(x))})
binary = binary[,c(2,3,4,9,21,22,23,1,5,6,7,8,11,12,13,19,15,17,20,25,16,18,14,24,10,26,27,28)]
z <- cor(binary)

rgb.palette <- colorRampPalette(c("red","white", "green"), space = "rgb")
levelplot(z,at = c(-1.1,-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9,1.1),aspect = "iso",col.regions=rgb.palette(120),scales=list(x=list(rot=90)),main="Missingness Correlation Matrix")

lmcar = LittleMCAR(scale(data[,3:ncol(data)]))

boxdata<-boxplot(scale(data[,3:ncol(data)]),
                 ylab ="z scores",
                 las = 2,
                 range = 15,
                 cex = 0.5
)



all_points = c()
#for each outlier in boxdata
for(i in 1:length(boxdata$group)){
  #add text to the boxplot
  points = which(scale(data[,3:ncol(data)])[,boxdata$group[i]]==boxdata$out[i])
  text(boxdata$group[i], boxdata$out[i], points,pos=4,cex=0.5)
  all_points = c(all_points,points)
}

sort(table(all_points))





complete = complete.cases(data)
companies = data[complete,1]
tickers = data[complete,2]
metrics = data[complete,c(3:ncol(data))]
response = data[complete,c(3:10)]
explanatory = data[complete,c(11:ncol(data))]


myPanel <- function(x, y, z, ...) {
  panel.levelplot(x,y,z,...)
  panel.text(x, y, z,cex =0.7)
}


paf.grid = round(paf(as.matrix(explanatory))$Anti.Image.Cor,2)
paf.list = melt(paf.grid)
corr.grid = round(cor(explanatory),2)
corr.list = melt(corr.grid)
rgb.palette <- colorRampPalette(c("red","white", "green"), space = "rgb")
levelplot(value ~ Var1*Var2, xlab="",ylab="",paf.list, panel = myPanel,at = c(-1.1,-0.7,0.7,1.1),aspect = "iso",col.regions=rgb.palette(120),scales=list(x=list(rot=90)),main="Anti-image Correlation Matrix")
levelplot(value ~ Var1*Var2, xlab="",ylab="",corr.list, panel = myPanel,at = c(-1.1,-0.3,0.3,1.1),aspect = "iso",col.regions=rgb.palette(120),scales=list(x=list(rot=90)),main="Correlation Matrix")




hist(explanatory)

PP = preProcess(explanatory, method = c("center", "scale","YeoJohnson")) #
zscores = predict(PP,explanatory)
hist(zscores)


VSS.scree(zscores)
VSS(zscores,fm="pc")
fa.parallel(zscores,fa="pc",n.iter=100)
abline(h=1)



factor.num = 4

set.seed(100)
fa = principal(zscores,nfactors=factor.num,rotate='varimax')
#coef <- solve(fa$correlation) %*% (fa$loadings)
scores=fa$scores
scores[,4] = -scores[,4]
scores[,3] = -scores[,3]

print(fa,sort=TRUE)

factor.names = c("Performance","Size","Stock Value","Liquidity")[1:factor.num]
colnames(scores) = factor.names







sizes = c()
wss <- (nrow(scores)-1)*sum(apply(scores,2,var))
for (i in 2:30) {
  km = kmeans(scores, 
              centers=i,nstart = 100)
  wss[i] <- sum(km$withinss)
 
}
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")








fit <- cascadeKM(scores, 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


cluster.num = 5

library(ade4)
set.seed(100)
km<-kmeans(scores,cluster.num,nstart = 100)
clusters.factor<-factor(km$cluster)
clusters = km$cluster


cluster.sizes = km$size
cluster.sizes
centers = km$centers
colnames(centers) = factor.names
centers

data.frame(cluster.sizes)

dist(centers)
scores.clusters = data.frame(cbind(scores,clusters))
scores.clusters[,'dist.from.centre'] = NA
euc.dist <- function(Var1, Var2) sqrt(sum((Var1 - Var2) ^ 2))
for (i in 1:nrow(scores.clusters)){
  c = scores.clusters[i,'clusters']
  d = euc.dist(scores.clusters[i,1:factor.num],centers[c,])
  scores.clusters[i,'dist.from.centre'] = d
}

boxplot(dist.from.centre~clusters, 
        data = scores.clusters,
        xlab = 'distance',
        ylab = 'cluster',
        main="Box Plot showing Euclidean Distance of \n Observations from Cluster Centres",
        horizontal = TRUE)



myPanel <- function(x, y, z, ...) {
  panel.levelplot(x,y,z,...)
  panel.text(x, y, z,cex =1)
}

centers.list = melt(centers)
rgb.palette <- colorRampPalette(c("red","white", "green"), space = "Lab")


levelplot(round(value,2) ~ Var1*Var2,centers.list,
          panel = myPanel,
          #at = c(-1.75,-1.25,-0.75,-0.25,0.25,0.75,1.25,1.75),
          at = c(-2,-1,-0.5,0.5,1,2),
          #at = c(-3,-1,1,3), 
          pretty=TRUE,aspect = "iso",
          col.regions=rgb.palette(5),
          scales=list(x=list(rot=0)),
          main="Cluster Centres",
          xlab = 'clusters',
          ylab = 'factors')


cluster.names = c('Stable Giants','Underperformers','Smaller Liquids','Bargain Stocks','Smaller Illiquids')[1:cluster.num]
row.names(centers) = cluster.names

companies[which(clusters==5)]



print(fa,sort=TRUE)
factor.variables = list(
  c('profitmargin','roe','roi','opermargin','price2sales','price2book')
  ,c('currentassets','currliabilities','sales','marketcap','avgvol','shortint')
  ,c('peratio','price2cash','eps')
  ,c('totdebt2equity','ltdeb2equity','currentratio','shortintratio','intexpense')
)

library(plyr)
cluster.summary = ddply(explanatory,.(factor(clusters)),numcolwise(median,na.rm = TRUE))
cluster.summary[,1]<-NULL
row.names(cluster.summary)=cluster.names
for (i in 1:4){
print (cluster.summary[,factor.variables[[i]]])
}



  

par(mfrow=c(factor.num,factor.num), mar=c(1,1,1,1)) 

for (i in 1:factor.num){
  for (j in 1:factor.num){
    if (i==j){
      plot.new()
      name = factor.names[i]
      text(0.5,0.5,name)
    }else{
plot(scores[,c(j,i)],cex=0.1)
s.class(scores[,c(j,i)],fac=clusters.factor,add.plot=TRUE, col=rainbow(nlevels(clusters.factor)))
}
}
}

par(mfrow=c(1,1),mar=c(5,5,3,3))



factors.matrix = diag(factor.num) * 2
row.names(factors.matrix) = factor.names
colnames(factors.matrix) = factor.names

MDSdata = rbind(scores,factors.matrix)
colours = clusters + 1
d <- dist(MDSdata)

mds <- cmdscale(d)
Dim1 <- mds [,1]
Dim2 <- mds [,2]

plot(Dim1[1:(length(Dim1)-factor.num)], Dim2[1:(length(Dim1)-factor.num)], xlab="", 
     ylab="", pch = 16, cex = 0.8, 
     main="MDS",col=colours,
     xlim=c(-2.5, 3)
     )
segments(-1500, -0, 1500, 0, lty="dotted")
segments(0, -1500, 0, 1500, lty="dotted")
text(Dim1[(length(Dim1)-(factor.num-1)):length(Dim1)], Dim2[(length(Dim2)-(factor.num-1)):length(Dim2)], factor.names, cex=1.2)

legend("topright",cluster.names,pch=16,col=2:(cluster.num+1))
 # gives the legend appropriate symbols (lines)

 # gives the legend lines the correct color and width







set.seed(100)
r = response[,1]
numerical.level = rep(0,312)
upper = quantile(r,c(2/3))
lower = quantile(r,c(1/3))
upper
lower
width = upper-lower
numerical.level[r>upper] = 1
numerical.level[r<= upper ] = 0
numerical.level[r< lower ] = -1

level =  c(' Low','  Medium','   High')[numerical.level+2]

hist(r,breaks = c(lower-4*width,
                  lower-3*width,
                  lower-2*width,
                  lower-1*width,
                  lower,
                  upper,
                  upper+1*width,
                  upper+2*width,
                  upper+3*width,
                  upper+4*width,
                  upper+5*width),
     col=c('red','red','red','red','grey','green','green','green','green','green'),
     main = 'Histogram of beta, coloured by category',
     xlab = 'beta')
#level[r> 1 ] = 1
#level[r<= 1 ] = -1


#level = rep(0,312)
#level[r>quantile(r,c(.5))] = 1
#level[r<= quantile(r,c(.5)) ] = -1


#level = clusters
scoresDF = data.frame(scores)
input = cbind(level, scoresDF)
input[,1]=factor(input[,1])

hist(scoresDF)

set.seed(100)
z<-lda(level~Performance+Size+Stock.Value+Liquidity,data = input,CV=FALSE)
CV<-lda(level~Performance+Size+Stock.Value+Liquidity,data = input,CV=TRUE)
preds = CV$class
#preds = predict(z, data.frame(scores))$class
z

z$counts
par(mfrow=c(2,2),mar=c(4.5,3,3,3))
boxplot(Performance~level,data=input, main="Performance",xlab='beta')
boxplot(Size~level,data=input, main="Size",xlab='beta')
boxplot(Stock.Value~level,data=input, main="Stock.Value",xlab='beta')
boxplot(Liquidity~level,data=input, main="Liquidity",xlab='beta')
par(mfrow=c(1,1),mar=c(5,5,3,3))

par(mfrow=c(2,2),mar=c(4.5,3,3,3))
ppPlot(scoresDF[,1],main='Performance',xlab="")
ppPlot(scoresDF[,2],main='Size',xlab="")
ppPlot(scoresDF[,3],main='Stock.Value',xlab="")
ppPlot(scoresDF[,4],main='Liquidity',xlab="")

par(mfrow=c(1,1),mar=c(5,5,3,3))

x = scoresDF
ina = numerical.level + 2
cov.Mtest(scoresDF,ina)

by(scores,level,cov)

lda.values = predict(z, data.frame(scores))

ldahist(data = lda.values$x[,1], level,xlab='LD1',main = 'LD1 histograms by category')


discPower(scoresDF,level)
discPower(lda.values$x,level)

confusionMatrix(preds,level)

#plot(lda.values$x[,1],lda.values$x[,2],
#     xlab = 'LD1', ylab = 'LD2'
#)
#     col=c('red','grey','green')[as.numeric(preds)],pch=16,

plot(lda.values$x[,1],lda.values$x[,2],
     col=c('red','grey','green')[numerical.level+2],pch=16,
     xlab = 'LD1', ylab = 'LD2'
)

plot(lda.values$x[,1],lda.values$x[,2],
     pch=c('o','x')[-as.integer(-as.numeric(preds)+4==(numerical.level+2))+2],
     col=c('yellow','black')[-as.integer(-as.numeric(preds)+4==(numerical.level+2))+2],
     xlab = 'LD1', ylab = 'LD2'
)





drawparti(lda.values$x[,1],lda.values$x[,2],grouping=as.factor(level),
          image.colors = c("green","grey","red"),
          prec=500, gs=rep(1,312),
          pch.mean = 19, cex.mean = 0, col.mean = "white",
          xlab = 'LD1',ylab='LD2', main = '',
          col.correct='black',
          col.wrong='black',
          imageplot = TRUE
          )





completeexplan = complete.cases(data[,c(11:ncol(data))])
completebeta = complete.cases(data[,'beta'])
testcases = which(completeexplan & !completebeta)
testcases.labels = data[testcases,1]
testcases.tickers = data[testcases,2]
explan.test = data[testcases,c(11:ncol(data))] 
zscores.test = predict(PP,explan.test)
scores.test = predict(fa,zscores.test)
scores.test[,4] = -scores.test[,4]
scores.test[,3] = -scores.test[,3]
colnames(scores.test) = factor.names
predict.test = predict(z,data.frame(scores.test))
classes.test = predict.test$class
probs.test = predict.test$posterior
lda.values.test = predict.test$x
test.certainty = apply(probs.test,1,max)

plot(lda.values.test[,1],lda.values.test[,2],
     col=c('red','grey','green')[-as.numeric(classes.test)+4],pch=16,
     xlab = 'LD1', ylab = 'LD2'
)

text (lda.values.test[,1],lda.values.test[,2],testcases.labels,cex=0.7,pos=1)


test.df = data.frame(testcases.labels,probs.test,classes.test,test.certainty)
names(test.df) = c('name','High','Medium','Low','pred','certainty')


dep = response[,5:8]
#corMat = cor(dep)
#melt.corMat = melt(corMat)
#levelplot(round(value,2) ~ Var1*Var2, at = c(-1,-.75,-.5,-.25,0,.25,.5,.75,1),xlab="",ylab="",melt.corMat, panel = myPanel,aspect = "iso",col.regions=rgb.palette(120),scales=list(x=list(rot=90)),main="Correlation Matrix")

par(mfrow=c(2,2),mar=c(4.5,3,3,3))
ppPlot(dep[,1],main='pctchg4wks',xlab="")
ppPlot(dep[,2],main='pctchg13wks',xlab="")
ppPlot(dep[,3],main='pctchg26wks',xlab="")
ppPlot(dep[,4],main='pctchg52wks',xlab="")

par(mfrow=c(1,1),mar=c(5,5,3,3))
hist(dep)

by(dep,cluster.names[clusters],cov)
cov.Mtest(dep,clusters)


input.MANOVA = data.frame(cluster= cluster.names[clusters],dep)
Y = as.matrix(dep)
fit <- manova(Y ~ cluster.names[clusters])
summary(fit, test="Pillai")
summary(fit, test="Wilks")
summary(fit, test="Hotelling-Lawley")
summary(fit, test="Roy")

means <- aggregate(input.MANOVA[,2:5],list(cluster = input.MANOVA$cluster), mean)
means

change.over.time = data.frame(cluster = means[,1], norm.price.52wks = rep(100,5))
change.over.time[,'norm.price.26wks'] = 100 * (100 + means[,'pctchg52wks']) / (100 + means[,'pctchg26wks'])
change.over.time[,'norm.price.13wks'] = 100 * (100 + means[,'pctchg52wks']) / (100 + means[,'pctchg13wks'])
change.over.time[,'norm.price.4wks'] = 100 * (100 + means[,'pctchg52wks']) / (100 + means[,'pctchg4wks'])
change.over.time[,'currentprice'] = 100 * (100 + means[,'pctchg52wks']) / (100 + 0)


#for (i in 2:5){
#  change.over.time[,i+1] = (change.over.time[,i] / (100 + means[,i]))  *100
#}

#names(change.over.time)[3:6] = c('norm.price.4wks','norm.price.13wks','norm.price.26wks','norm.price.52wks')

melt.change.over.time = melt(change.over.time)
melt.change.over.time[,'variable'] = as.integer(melt.change.over.time[,'variable'])
melt.change.over.time[melt.change.over.time[,'variable']==5,'variable'] = 0
melt.change.over.time[melt.change.over.time[,'variable']==4,'variable'] = 4
melt.change.over.time[melt.change.over.time[,'variable']==3,'variable'] = 13
melt.change.over.time[melt.change.over.time[,'variable']==2,'variable'] = 26
melt.change.over.time[melt.change.over.time[,'variable']==1,'variable'] = 52

ggplot(
  melt.change.over.time,
  aes(x=variable, y=value, color=cluster,group=cluster) 
)  + geom_point(shape=1) + geom_line() +theme(legend.position="right") + xlab("Weeks Ago") + ylab("Normalised Stock Price") + ggtitle("Normalised Stock Prices over Time grouped by cluster") + scale_x_reverse()


#parcoord(change.over.time[,3:6],col=1:5,lwd=5)

#parcoord(dep,col=clusters,lwd=5)