#Data Preprocessing
dataset2 <- read.csv("C:/Users/rushi/Desktop/IDM/dataset2.csv")
dataset2n <- scale(dataset2)

wss2 <- (nrow(dataset2)-1)*sum(apply(dataset2,2,var))
wss2n <- (nrow(dataset2n)-1)*sum(apply(dataset2n,2,var))
for(i in 2:15) wss2[i] <- sum(kmeans(dataset2,centers = i,nstart = 3)$withinss)
for(i in 2:15) wss2n[i] <- sum(kmeans(dataset2n,centers = i,nstart = 3)$withinss)
plot(1:15,wss2,type = "b", xlab = "Number of Clusters",ylab = "Within Group SS" )
plot(1:15,wss2n,type = "b", xlab = "Number of Clusters",ylab = "Within Group SS" )


#K means clustering
kc2 <- kmeans(dataset2,10)
kc2n <- kmeans(dataset2n,10)

#Plot 3d graph
install.packages("rgl")
library(rgl)
plot3d(dataset2, col=kc2$cluster)
plot3d(dataset2, col=kc2n$cluster)