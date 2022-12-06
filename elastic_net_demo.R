# TO DO: Give an overview of this demo.
library(glmnet)
library(parallel)
source("cv.R")

# Initialize the sequence of pseudorandom numbers.
set.seed(1)

# Simulate a regression data set.
n <- 200
p <- 1000
X <- matrix(rnorm(n*p),n,p)
b <- rep(0,p)
b[sample(p,4)] <- c(1,-1,1,-1)
y <- X %*% b + rnorm(n)

# Fit an Elastic Net model using glmnet.
fit_glmnet <- function (x, y, cvpar, noncvpar, init)
  glmnet(x,y,lambda = cvpar,alpha = 0.5)

# Predict Y using the fitted Elastic Net model.
predict_glmnet <- function (x, model)
  predict(model,x)

# Compute the mean squared error (MSE) between the predicted Y and
# true Y.
compute_mse <- function (pred, true)
  mean((pred - true)^2)

# Perform cross-validation to choose the penalty strength of the
# Elastic Net model.
lambda <- round(rev(exp(seq(-3.75,0.85,length.out = 100))),digits = 4)
t0 <- proc.time()
cv <- perform_cv(fit_glmnet,predict_glmnet,compute_mse,X,y,lambda,nc = 1)
t1 <- proc.time()
print(t1 - t0)

# Compare with cv.glmnet.
res <- cv.glmnet(X,y,alpha = 0.5)
plot(res$lambda,res$cvm,type = "l",col = "darkblue",lwd = 2,log = "x",
     xlab = "lambda",ylab = "mse")
lines(lambda,rowMeans(cv),col = "darkorange",lwd = 2,lty = "dashed")
