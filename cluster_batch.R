rm(list = ls(all.names = TRUE))
library(leaflet.extras)
library(apcluster)
load(file = "apres2.rda")
load(file = "x2-13000.rda")
head(x2)
dim(x2)
plot(apres, x2)


predict.apcluster <- function(s, exemplars, newdata)
{
  simMat <- s(rbind(exemplars, newdata), sel=(1:nrow(newdata)) + nrow(exemplars))[1:nrow(exemplars), ]
  unname(apply(simMat, 2, which.max))
}
resultado <- list()

setwd("~/r-files/AffinityPropagationClustering/")
filenames <- list.files(path = "~/r-files/AffinityPropagationClustering/geo/") 
filenames
setwd("~/r-files/AffinityPropagationClustering/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("~/r-files/AffinityPropagationClustering/")
head(data)
names(data)
dados <- cbind(data$long, data$lat, as.character(data$default), as.character(data$tipo))
dados <- as.data.frame(dados)
dados$V1 <- as.numeric(as.character(dados$V1))
dados$V2 <- as.numeric(as.character(dados$V2))
dados <- dados[dados$V2 < 0, ]
dados <- subset(dados, !is.na(V1))
dim(dados)
dados$newrow <- sample(1000, size = nrow(dados), replace = TRUE)
dados$cut <- cut(dados$newrow, breaks=seq(0,1000,200), labels=sprintf("Score %d-%d",seq(0, 800, 200), seq(200,1000,200)))


dados$cluster = 0
head(dados)
tail(dados)
dadosfim = dados[1:103041,]

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

a = 0
meucluster((a=a+1))

var = lapply(1:5, meucluster)
var

head(dados)
cluster = c(1:2)
dadosc = dados[dados$cluster == cluster,]
dadosc
tamanho = length(dados$cluster[dados$cluster==cluster])
tamanho

pal <- colorFactor(
  palette = 'Dark2',
  domain = dados$cluster
)

leaflet(dados) %>%
  addTiles(group="OSM") %>% 
  addCircles(~V1, ~V2, weight = 0.1, radius=30, color=~pal(cluster),
             stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topright", colors= "blue", labels=paste("Alvaras"), title="Cluster")


#######################
#######################

pal <- colorFactor(
  palette = 'Dark2',
  domain = dados$cluster
)

leaflet(dados) %>%
  addTiles(group="OSM") %>% 
  addCircles(~V1, ~V2, weight = 0.1, radius=30, color=~pal(cluster),
             stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topright", colors= "blue", labels=paste("alvaras", sep = " "), title="Cluster")
