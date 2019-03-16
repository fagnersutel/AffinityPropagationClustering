rm(list = ls(all.names = TRUE))
library(apcluster)
setwd("~/AffinityPropagationClustering/")
filenames <- list.files(path = "~/AffinityPropagationClustering/geo/c") 
filenames
setwd("~/AffinityPropagationClustering/geo/c") 
data1 <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("~/AffinityPropagationClustering/")
head(data1)
filenames <- list.files(path = "~/AffinityPropagationClustering/geo/s") 
filenames
setwd("~/AffinityPropagationClustering/geo/s") 
data2 <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("~/AffinityPropagationClustering/")
head(data2)
data = rbind(data1, data2)
names(data)
dim(data)
dados <- cbind(data$long, data$lat, as.character(data$default), as.character(data$tipo))
dados <- as.data.frame(dados)
dados$V1 <- as.numeric(as.character(dados$V1))
dados$V2 <- as.numeric(as.character(dados$V2))
dados <- dados[dados$V2 < 0, ]
dados <- subset(dados, !is.na(V1))
dim(dados)
rm(data)
rm(filenames)

x2 <- cbind(dados$V1, dados$V2)
x2 <- x2[complete.cases(x2), ]
dim(x2)
head(x2)
#x1 <- x2[1:5000,]
x1 <- x2
x2 <- x2[sample(nrow(x2), 50000), ]
save(x2, file = "x2-50000.rda")
dim(x1)
dim(x2)
#apres <- apcluster(negDistMat(r=2), x2, q=0)
apres <- apcluster(negDistMat(r=2), x2, q=0.55)
#plot(apres, x2)
summary(apres)

save(apres, file = "apres2.rda")
install.packages('beepr')
library(beepr)
beep()

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
    addLegend("topright", colors= "blue", labels=paste("Cluster:", cluster, ", com", tamanho, "alvar?s", sep = " "), title="Cluster")
}

save(dados, file = "clusters_q06_13000clusters.rda")
#write.csv(dados, "clusters_q07_94clusters.csv", row.names=FALSE)

#######################
#######################
