## Example
# Data
x <- rbind(matrix(rnorm(70000, sd = 0.3), ncol = 2),
           matrix(rnorm(70000, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")

# CAH without kmeans : dont work necessarily
library(FactoMineR)
cah.test <- HCPC(x, graph=FALSE, nb.clust=-1)

# CAH with kmeans : work quickly
cl <- kmeans(x, 1000, iter.max=20)
cah <- HCPC(cl$centers, graph=FALSE, nb.clust=-1)
plot.HCPC(cah, choice="tree")




## Generate data
set.seed(2017)
x = c(rnorm(250000, 0,0.9), rnorm(350000, 4,1), rnorm(500000, -5,1.1))
y = c(rnorm(250000, 0,0.9), rnorm(350000, 5.5,1), rnorm(500000,  5,1.1))
XY = data.frame(x,y)
Sample5K = sample(length(x), 5000)     ## Downsample

## Cluster the sample
DM5K = dist(XY[Sample5K,])
HC5K = hclust(DM5K, method="single")
Groups = cutree(HC5K, 8)
Groups[Groups>4] = 4
plot(XY[Sample5K,], pch=20, col=rainbow(4, alpha=c(0.2,0.2,0.2,1))[Groups])



#
dados$V3 = NULL
dados$V4 = NULL
head(dados)
x = dados$V1
y = dados$V2
XY = data.frame(x,y)
dim(XY)
Sample5K = sample(length(x), 5000)     ## Downsample

## Cluster the sample
DM5K = dist(XY[Sample5K,])
HC5K = hclust(DM5K, method="single")
View(HC5K)
Groups = cutree(HC5K, 51)
#Groups[Groups>4] = 4
plot(XY[Sample5K,], pch=20, col=rainbow(51, alpha=c(0.2,0.2,0.2,1))[Groups])
teste = XY[Sample5K,]
head(teste, 50)
