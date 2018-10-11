rm(list = ls(all.names = TRUE))
library(leaflet.extras)
library(apcluster)
load(file = "apres2.rda")
dados = read.csv('clusters_q07_94clusters2.csv', header = TRUE, sep = ",")
head(dados)
x2 <- cbind(dados$V1, dados$V2)
x2 <- x2[complete.cases(x2), ]
head(x2)
x1 <- x2[1:5000,]
x1 <- x2
x2 <- x2[sample(nrow(x2), 5000), ]
dim(x1)
dim(x2)
plot(apres, x2)


predict.apcluster <- function(s, exemplars, newdata)
{
  simMat <- s(rbind(exemplars, newdata), sel=(1:nrow(newdata)) + nrow(exemplars))[1:nrow(exemplars), ]
  unname(apply(simMat, 2, which.max))
}
resultado <- list()

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

meucluster(1)
meucluster(5)
meucluster(c(1,2,3))
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
  addLegend("topright", colors= "blue", labels=paste("com", tamanho, "alvaras", sep = " "), title="Cluster")


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
