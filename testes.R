A<-c(1999,1999, 2001, 2001)
B<-c("A", "D", "A", "D")
df<-data.frame (A, B)
df
df$C<-apply(df[1], 2, function(x) ifelse(x > 2000, 2000, x))
df


# create a matrix of 10 rows x 2 columns
m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
m
m = as.data.frame(m)
# mean of the rows
m$mean <- apply(m, 1, mean)
m

dados$cluster <- apply(dados, 1, predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ],  dados[, 1:2])

apply(dados[, 1:2], 1, predict.apcluster(negDistMat(r=2), x2[apres@exemplars, ],  dados[, 1:2]))                       

aa= length(dados$V1)/1000
aa = aa+1
for(i in 1:aa) {
  print(i)
}



