# TO DO: Give an overview of this demo.
library(glmnet)
source("cv.R")

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# Simulate a regression data set.
n <- 200
p <- 1000
X <- matrix(rnorm(n*p),n,p)
b <- rep(0,p)
b[sample(p,4)] <- c(1,-1,1,-1)
y <- drop(X %*% b + rnorm(n))

# Fit an Elastic Net model using glmnet.
fit_glmnet <- function (dat, cvpar)
  glmnet(X = dat[,-1],y = dat[,1],lambda = cvpar,alpha = 0.5)

# Compute the root mean squared error.
rmse <- function (pred, true) {

}

# Perform cross-validation to choose the penalty strength of the
# Elastic Net model.
lambda <- rev(exp(seq(-4.5,0.2,length.out = 100)))
res <- perform_cv(fit_glmnet,compute_rmse,cbind(y,X),lambda)

# Compare with cv.glmnet.
# TO DO.

