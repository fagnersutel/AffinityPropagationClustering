rm(list = ls(all.names = TRUE))
library(leaflet.extras)
library(apcluster)
setwd("~/r-files/AffinityPropagationClustering/")
filenames <- list.files(path = "~/r-files/AffinityPropagationClustering/geo/") 
filenames
setwd("~/r-files/AffinityPropagationClustering/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("~/r-files/AffinityPropagationClustering/")
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

x2 <- cbind(dados$V1, dados$V2)
x2 <- x2[complete.cases(x2), ]
head(x2)
#x1 <- x2[1:5000,]
x1 <- x2
x2 <- x2[sample(nrow(x2), 5000), ]
dim(x1)
dim(x2)
#apres <- apcluster(negDistMat(r=2), x2, q=0)
apres <- apcluster(negDistMat(r=2), x2, q=0.7)
plot(apres, x2)
summary(apres)

save(apres, file = "apres2.rda")
#load(file = "apres.rda")
#plot(apres, x2)


predict.apcluster <- function(s, exemplars, newdata)
{
  simMat <- s(rbind(exemplars, newdata), sel=(1:nrow(newdata)) + nrow(exemplars))[1:nrow(exemplars), ]
  unname(apply(simMat, 2, which.max))
}
resultado <- list()
resul <- predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ], x1[1:1000,])
length(resul)
head(resul)
teste <- as.data.frame(resul)
dim(teste)

#dadosteste = dados[1:1000, 1:2]
#head(dadosteste)
#dados$cluster = 0
#head(dados)

#for (posicao in dadosteste){
#  dados$cluster[posicao] = predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ],  dadosteste[posicao, 1:2])
#}
dados$cluster = 0
head(dados)
tail(dados)
dadosfim = dados[1:103041,]

#dadosfim = dados[1:2000,]
head(dadosfim)
length(dadosfim$V1)
aa= length(dadosfim$V1)/1000
aa = aa+1
aa
for(i in seq(from=1, to=length(dadosfim$V1)-1000, by=1000)){
  #print(paste(i, (i+999), sep = " - "))
  inicio = i
  final = i+999
  print(paste(inicio, final, sep = " - "))
  resultado = predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ],  dadosfim[inicio:final, 1:2])
  dados$cluster[inicio:final] = resultado
}

length(resultado)
head(dados)
tail(dados, 50)

meucluster <- function(cluster) {
  dadosc = dados[dados$cluster == cluster,]
  tamanho = length(dados$cluster[dados$cluster==cluster])
  leaflet(dadosc) %>%
    addTiles(group="OSM") %>% 
    addCircles(~V1, ~V2, weight = 0.1, radius=8, color= 'blue',
               stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("topright", colors= "blue", labels=paste("com", tamanho, "alvar?s", sep = " "), title="Cluster")
}

save(dados, file = "clusters_q07_94clusters.rda")
write.csv(dados, "clusters_q07_94clusters.csv", row.names=FALSE)
meucluster(1)

meucluster(5)

meucluster(c(1,2,3))
meucluster(4)

meucluster(11)

meucluster(18)

meucluster(22)

meucluster(29)

meucluster(41)

meucluster(45)
meucluster(c(41,45))

meucluster(44)

meucluster(46)


meucluster(39)
meucluster(50)
meucluster(42)
meucluster(c(39,42,50))



a = 0
meucluster((a=a+1))

df2 = data.frame()
meucluster <- function(cluster) {
  tamanho = length(unique(cluster))
  df2 = data.frame(color = topo.colors(tamanho, alpha = NULL), stringsAsFactors = FALSE)
  dadosc = dados[dados$cluster == cluster,]
  leaflet(dadosc) %>%
    addTiles(group="OSM") %>% 
    addCircles(~V1, ~V2, weight = 0.1, radius=1, color=~cluster,
               stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("bottomright", colors= "red", labels="Porto Alegre", title="Clusters")
}


cluster = c(1:3)

tamanho = length(unique(cluster))
tamanho
df2 = data.frame(color = topo.colors(tamanho, alpha = NULL), stringsAsFactors = FALSE)
df2
dadosc = dados[dados$cluster == cluster,]
leaflet(dadosc) %>%
  addTiles(group="OSM") %>% 
  addCircles(~V1, ~V2, weight = 0.1, radius=10, color=c('red', 'blue', 'green'),
             stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomright", colors= "red", labels="Porto Alegre", title="Clusters")






load(file = "apres.rda")
dados = read.csv('clusters_q07_94clusters.csv', header = TRUE, sep = ",")
head(dados)
x2 <- cbind(dados$V1, dados$V2)
x2 <- x2[complete.cases(x2), ]
head(x2)
#x1 <- x2[1:5000,]
x1 <- x2
x2 <- x2[sample(nrow(x2), 5000), ]
dim(x1)
dim(x2)
plot(apres, x1)

###################
###################
###################
###################


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
