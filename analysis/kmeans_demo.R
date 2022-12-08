source("../code/cv.R")
set.seed(1)

# Simulate a clustering data set.
n <- 200
k <- 5
centers <- matrix(rnorm(2*k),k,2)
membership <- sample(k,n,replace = TRUE)
X <- matrix(0,n,2)
for (i in 1:n) {
  j     <- membership[i]
  X[i,] <- centers[j,] + rnorm(2)/3
}
plot(X[,1],X[,2],col = "royalblue",pch = 1,cex = 0.75,xlab = "x1",ylab = "x2")
points(centers[,1],centers[,2],col = "red",pch = 20,cex = 1)

# This function runs k-means.
run_kmeans <- function (x, y, cvpar, noncvpar, init)
  kmeans(x,init[1:cvpar,],iter.max = 100)

# Choose the "best-fit" cluster centers for the data points x.
predict_kmeans <- function (x, model)
  predict(model,x)

# This function computes the mean squared error (MSE) between the
# estimated cluster centers and the true cluster centers.
compute_mse <- function (pred, true)
  mean((pred - true)^2)
