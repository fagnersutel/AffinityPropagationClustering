#https://stackoverflow.com/questions/20621250/simple-approach-to-assigning-clusters-for-new-data-after-k-means-clustering
library("flexclust")
data("Nclus")

set.seed(1)
dat <- as.data.frame(Nclus)
ind <- sample(nrow(dat), 50)

dat[["train"]] <- TRUE
dat[["train"]][ind] <- FALSE

cl1 = kcca(dat[dat[["train"]]==TRUE, 1:2], k=51, kccaFamily("kmeans"))
cl1    
#
# call:
# kcca(x = dat[dat[["train"]] == TRUE, 1:2], k = 4)
#
# cluster sizes:
#
#  1   2   3   4 
#130 181  98  91 

pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])

image(cl1)
points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch=19, cex=0.3)
points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")
as.kcca(cl1, data=x)

###############################################
dat <- as.data.frame(dados)
ind <- sample(nrow(dat), 10000)

dat[["train"]] <- TRUE
dat[["train"]][ind] <- FALSE

cl1 = kcca(dat[dat[["train"]]==TRUE, 1:2], k=51, kccaFamily("kmeans"))
cl1    

pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])

image(cl1)
points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch='.', cex=0.3)
#points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")
points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch='*', bg="orange", cex=0.1)
