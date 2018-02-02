#Data Preprocessing
dataset1 <- read.csv("C:/Users/rushi/Desktop/IDM/dataset1.csv") 
dataset1.3 <- dataset1[,-c(4)]
dataset1.3n <- scale(dataset1.3)
distance <- dist(dataset1.3)
distancen <- dist(dataset1.3n)

#Hierarchical Clustering
hc.c <- hclust(distance)
hc.cn <- hclust(distancen)
plot(hc.c)
plot(hc.cn)
member.c <- cutree(hc.c,8)
member.cn <- cutree(hc.cn,8)
distance2 <- dist(dataset1[4])
hc.c2 <- hclust(distance2)
plot(hc.c2)
member.c2 <- cutree(hc.c2,8)

wss <- (nrow(dataset1.3)-1)*sum(apply(dataset1.3,2,var))
wssn <- (nrow(dataset1.3n)-1)*sum(apply(dataset1.3n,2,var))
for(i in 2:8) wss[i] <- sum(kmeans(dataset1.3,centers = i)$withinss)
for(i in 2:8) wssn[i] <- sum(kmeans(dataset1.3n,centers = i)$withinss)
plot(1:8,wss,type = "b", xlab = "Number of Clusters",ylab = "Within Group SS" )
plot(1:8,wssn,type = "b", xlab = "Number of Clusters",ylab = "Within Group SS" )

#k means Clustering
kc <- kmeans(dataset1.3,8)
kcn <- kmeans(dataset1.3n,8)
plot(x~y,dataset1,col = kc$cluster)
plot(x~y,dataset1,col = kcn$cluster)

#density based
install.packages("dbscan")
library("dbscan")
hdb <- hdbscan(dataset1.3, minPts = 12)
hdb
hdbn <- hdbscan(dataset1.3n, minPts = 13)
hdbn

#Graph based Clustering
snn <- sNNclust(distance,k = 20,eps = 10,minPts = 16)
snn
snnn <- sNNclust(distancen,k = 15,eps = 11,minPts = 13)
snnn

#3d plot
install.packages("rgl")
library(rgl)
plot3d(dataset1, col=kc$cluster)
plot3d(dataset1, col=kcn$cluster)
plot3d(dataset1, col=member.c)
plot3d(dataset1, col=member.cn)

#Calculating root-mean-square-deviation
install.packages("Metrics")
library(Metrics)

rmse(kc$cluster,distance2)
rmse(kcn$cluster,distance2)
rmse(snn$cluster,distance2)
rmse(snnn$cluster,distance2)
rmse(member.c,distance2)
rmse(member.cn,distance2)
rmse(hdb$cluster,distance2)
rmse(hdbn$cluster,distance2)

#Calculating Accuracies
t1 <- table(hdb$cluster,member.c2)
accuracyhdb <- sum(diag(t1))/sum(t1)

t2 <- table(hdbn$cluster,member.c2)
accuracyhdbn <- sum(diag(t1))/sum(t1)

t3 <- table(snn$cluster,member.c2)
accuracysnn <- sum(diag(t1))/sum(t1)

t4 <- table(snnn$cluster,member.c2)
ccuracysnnn <- sum(diag(t1))/sum(t1)

t5 <- table(kc$cluster,member.c2)
accuracykc <- sum(diag(t1))/sum(t1)

t6 <- table(kcn$cluster,member.c2)
accuracykcn <- sum(diag(t1))/sum(t1)

t7 <- table(member.c,member.c2)
accuracyhc <- sum(diag(t1))/sum(t1)

t8 <- table(member.cn,member.c2)
accuracyhcn <- sum(diag(t1))/sum(t1)
