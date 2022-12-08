source("../code/cv.R")
set.seed(1)

# Simulate a clustering data set.
n <- 400
k <- 5
centers <- matrix(rnorm(2*k),k,2)
membership <- sample(k,n,replace = TRUE)
X <- matrix(0,n,2)
for (i in 1:n) {
  j     <- membership[i]
  X[i,] <- centers[j,] + rnorm(2)/4
}
plot(X[,1],X[,2],col = "royalblue",pch = 1,cex = 0.75,xlab = "x1",ylab = "x2")
points(centers[,1],centers[,2],col = "red",pch = 20,cex = 1)

# Run k-means once with 10 clusters.
fit_k10 <- kmeans(X,centers = 10,iter.max = 100)

# This function runs k-means, initializing the cluster centers using
# the k-means clustering result with k = 10 clusters.
run_kmeans <- function (x, y, cvpar)
  kmeans(x,fit_k10$centers[1:cvpar,],iter.max = 100)

# k <- 5
# res <- apply(out,2,which.min)
# plot(X[,1],X[,2],col = res,pch = 1,cex = 0.75,xlab = "x1",ylab = "x2")
# points(fit_k10$centers[,1],fit_k10$centers[,2],pch = 4,cex = 1)

# Choose the "best-fit" cluster centers for the data points x.
predict_kmeans <- function (x, model) {
  k <- nrow(model$centers)
  D <- as.matrix(dist(rbind(model$centers,x)))
  D <- D[1:k,-(1:k)]
  i <- apply(D,2,which.min)
  return(model$centers[i,])
}

# This function computes the mean squared error (MSE) between the
# estimated cluster centers and the true cluster centers.
compute_mse <- function (pred, true)
  mean((pred - true)^2)

# Having defined these three functions, we are ready to use
# perform_cv.
k <- 2:10
t0 <- proc.time()
cv <- perform_cv(run_kmeans,predict_kmeans,compute_mse,X,
                 centers[membership,],k,nc = 1)
t1 <- proc.time()
print(t1 - t0)

# Plot k vs. MSE.
par(mar = c(4,4,0,0))
plot(k,rowMeans(cv),type = "l",col = "darkblue",lwd = 2,
     xlab = "k",ylab = "mse")
points(k,rowMeans(cv),col = "darkblue",pch = 20)
