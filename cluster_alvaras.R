rm(list = ls(all.names = TRUE))

library(apcluster)
setwd("/Users/fagnersuteldemoura/r-files/aae/")
filenames <- list.files(path = "/Users/fagnersuteldemoura/r-files/aae/geo/") 
filenames
setwd("/Users/fagnersuteldemoura/r-files/aae/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("/Users/fagnersuteldemoura/r-files/aae/")
head(data)
names(data)
dados <- cbind(data$long, data$lat, data$default, data$tipo)
dados <- as.data.frame(dados)
dados$V1 <- as.numeric(as.character(dados$V1))
dados$V2 <- as.numeric(as.character(dados$V2))
dados <- dados[dados$V2 < 0, ]
dados <- subset(dados, !is.na(V1))
dim(dados)
dados$newrow <- sample(1000, size = nrow(dados), replace = TRUE)
dados$cut <- cut(dados$newrow, breaks=seq(0,1000,200), labels=sprintf("Score %d-%d",seq(0, 800, 200), seq(200,1000,200)))
rm(data)
rm(filenames)
library(apcluster)
#cl1 <- cbind(rnorm(30, 0.3, 0.05), rnorm(30, 0.7, 0.04))
#cl2 <- cbind(rnorm(30, 0.7, 0.04), rnorm(30, 0.4, .05))
#x1 <- rbind(cl1, cl2)
#x1
#plot(x1, xlab="", ylab="", pch=19, cex=0.8)
#apres1a <- apcluster(negDistMat(r=2), x1)
#s1 <- negDistMat(x1, r=2)
#apres1b <- apcluster(s1)
#apres1a
#plot(apres1a, x1)
#heatmap(apres1a)
#heatmap(apres1b, s1)
#apres1c <- apcluster(s1, details=TRUE)
#plot(apres1c)
#cl3 <- cbind(rnorm(20, 0.50, 0.03), rnorm(20, 0.72, 0.03))
#cl4 <- cbind(rnorm(25, 0.50, 0.03), rnorm(25, 0.42, 0.04))
#x2 <- rbind(x1, cl3, cl4)
#x2 <- rbind(x1, cl3, cl4)
#x2
#plot(x2, xlab="", ylab="", pch=19, cex=0.)
#apres2a <- apcluster(negDistMat(r=2), x2)
#plot(apres2a, x2)
#apres2b <- apcluster(negDistMat(r=2), x2, q=0)
#plot(apres2b, x2)
#apres2c <- apcluster(negDistMat(r=2), x2, q=0.8)
#plot(apres2c, x2)
#apres2c@p     
#heatmap(apres2c)     




x2 <- cbind(dados$V1, dados$V2)
x2 <- x2[complete.cases(x2), ]
head(x2)
#x1 <- x2[1:5000,]
x1 <- x2
x2 <- x2[sample(nrow(x2), 3000), ]
dim(x1)
dim(x2)
plot(x2, xlab="", ylab="", pch=19, cex=0.2)
apres2a <- apcluster(negDistMat(r=2), x2)
plot(apres2a, x2)
heatmap(apres2a)
apres2b <- apcluster(negDistMat(r=2), x2, q=0)
plot(apres2b, x2)
apres2c <- apcluster(negDistMat(r=2), x2, q=0.8)
plot(apres2c, x2)
apres2c@p     
heatmap(apres2c)  
setwd("/Users/fagnersuteldemoura/r-files/aae/")
save(apres2a, file = "mydata.rda")
load(file = "mydata.rda")
plot(apres2a, x2)
     




#apres <- apcluster(negDistMat(r=2), x2, q=0)
apres <- apcluster(negDistMat(r=2), x2)
save(apres, file = "apres.rda")
load(file = "apres.rda")
plot(apres, x2)
summary(apres)
View(apres)
## auxiliary predict() function
predict.apcluster <- function(s, exemplars, newdata)
{
  simMat <- s(rbind(exemplars, newdata), sel=(1:nrow(newdata)) + nrow(exemplars))[1:nrow(exemplars), ]
  unname(apply(simMat, 2, which.max))
}
#resul <- predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ], x1[1:1000,])
resul <- predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ], x1[1:1000,])
dim(resul)
length(resul)
head(resul)
teste <- as.data.frame(resul)
dim(teste)
final <- cbind(x1, teste)
head(final,25)
dim(x1)
x3 <- x1[1:5000,]
x3 <- as.data.frame(x3)
dim(x3)
head(x3)
resultado <- list()
for (posicao in x3){
  resultado = predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ], x3[posicao,])
}



#Standart normal have variance 1; means of n standart normal have standart deviation 1/sqrt(n)
nosim = 1000
n = 10
matriz =matrix(rnorm(nosim * n), nosim)
dim(matriz)
head(matriz)
media = apply(matriz, 1, mean)
media
length(media)
sd(media)
1/sqrt(10)

#Standart uniforms have bariance 1/12; means of randon samples of n uniforms have sd 1/sqrt(12 * n)
nosim = 1000
n = 10
matriz =matrix(runif(nosim * n), nosim)
dim(matriz)
head(matriz)
media = apply(matriz, 1, mean)
media
length(media)
sd(media)
1/sqrt(12 * n)
