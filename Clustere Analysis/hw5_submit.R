install.packages("MASS")
library(MASS)
library(psych)
?UScereal
data = UScereal
head(data)
str(data)
plot(data)


## log transformation
data$logcalories = log(data$calories+1)
data$logprotein = log(data$protein+1)
data$logfat = log(data$fat+1)
data$logsodium = log(data$sodium+1)
data$logfibre = log(data$fibre+1)
data$logcarbo = log(data$carbo+1)
data$logsugars = log(data$sugars+1)
data$logpotassium = log(data$potassium+1)
plot(data[, 12:19])
round(cor(data[,12:19]),3)


## LOG DATA
logdata = data.frame(data[c(11:19)])
head(logdata)
str(logdata)
logdata$mfr = as.numeric(logdata$mfr)
logdata$vitamins = as.numeric(logdata$vitamins)

table(data$mfr)

## PRCOMP
fit2 = prcomp(logdata, scale=T)
plot(fit2) #scree plot
abline(h=1)
pairs(fit2$x)
summary(fit2)
round(fit2$rotation[, 1:4], 4)

fit2$rotation
fit2$sdev
fit2$sdev^2
sum(fit2$sdev^2)
fit2$sdev^2/sum(fit2$sdev^2)
fit2$x
round(var(fit2$x), 6)


## PERCEPTUAL MAP
plot(fit2$x[,1:2], xlim=c(-6.0,3), pch=16, col = data$mfr)
#G=General Mills, K=Kelloggs, N=Nabisco, P=Post, Q=Quaker Oats, R=Ralston Purina.
text(fit2$x[c(-3,-4,-12,-13),1]-.1,
     fit2$x[c(-3,-4,-12,-13),2],
     data$mfr[c(-3,-4,-12,-13)], adj=1, cex=.6)
text(fit2$x[c(3,4,12,13),1]+.1,
     fit2$x[c(3,4,12,13),2],
     data$mfr[c(3,4,12,13)], adj=0, cex=.6)
abline(h=0, v=0)

#by cereal name
plot(fit2$x[,1:2], xlim=c(-6.0,3), pch=16, col = data$mfr)
text(fit2$x[c(-3,-4,-12,-13),1]-.1,
     fit2$x[c(-3,-4,-12,-13),2],
     rownames(data)[c(-3,-4,-12,-13)], adj=1, cex=.5)
text(fit2$x[c(3,4,12,13),1]+.1,
     fit2$x[c(3,4,12,13),2],
     rownames(data)[c(3,4,12,13)], adj=1, cex=.5)



### CLUSTERING
Zdata = scale(logdata[,]) # standardize data
set.seed(12345)  

### Cluster 5
fit.th5 = kmeans(Zdata, 5, nstart=100)
fit8 = prcomp(Zdata)
summary(fit8)
plot(fit8)
round(fit8$rotation[,1:2],4)
mu=fit.th5$centers %*% fit8$rotation[,1:2]
plot(fit8$x[,1:2], col=fit.th5$cluster+1, pch=16)

text(fit8$x[c(-3,-4,-12,-13),1]-.1,
     fit8$x[c(-3,-4,-12,-13),2],
     data$mfr[c(-3,-4,-12,-13)], adj=1, cex=.8)

points(mu, cex=4, pch="X")


### Cluster 4
fit.th4 = kmeans(Zdata, 4, nstart=100)
fit9 = prcomp(Zdata)
summary(fit9)
plot(fit9)
round(fit9$rotation[,1:2],4)
mu=fit.th4$centers %*% fit9$rotation[,1:2]
plot(fit9$x[,1:2], col=fit.th4$cluster+1, pch=16)
text(fit9$x[c(-3,-4,-12,-13),1]-.1,
     fit9$x[c(-3,-4,-12,-13),2],
     data$mfr[c(-3,-4,-12,-13)], adj=1, cex=.8)


## Cluster 3
fit.th3 = kmeans(Zdata, 3, nstart=100)
fit10 = prcomp(Zdata)
summary(fit)
plot(fit)
round(fit10$rotation[,1:2],4)
mu=fit.th3$centers %*% fit10$rotation[,1:2]
plot(fit10$x[,1:2], col=fit.th3$cluster+1, pch=16)
text(fit10$x[c(-3,-4,-12,-13),1]-.1,
     fit10$x[c(-3,-4,-12,-13),2],
     data$mfr[c(-3,-4,-12,-13)], adj=1, cex=.8)

points(mu, cex=4, pch="X")

##TREE
#Regression Tree
install.packages("tree")
library(tree)
head(data)
Zdatatree = scale(data[,]) # standardize data
set.seed(12345)  
fit2 = tree(as.factor(shelf) ~ mfr + vitamins + logcalories + logprotein + logfat+ logsodium+ 
              logfibre+logcarbo+logsugars + logpotassium, data, mindev=1e-6) 
fit12 = prune.tree(fit2, best=8) 
par(mfrow=c(1,2))
plot(fit12, type="uniform")
text(fit12, cex=.8)
partition.tree(fit12, cex=.8) 
par(mfrow=c(1,1))
fit12


## Shelf by cluster
shelf <- data.frame(data$shelf, fit.th3$cluster)
table(fit.th3$cluster)
table(shelf)


######