# This is the main "perform_cv" interface for performing k-fold
# cross-validation. To learn how to use it, it is best to work through
# the examples (Elastic Net demo, k-means demo), then read the more
# detailed description of the inputs and outputs here.
#
# ARGUMENNTS
#
# fit: Function used to train the model. It should be a function of
# the form fit(x,y,cvpar,
#
# predict:
#
# evaluate: Function used to evaluate
#
# x: Training data, stored as an n x p matrix, where n is the number
# of data examples, and p is the number of training features.
#
# y: Test data, stored as an n x p1 matrix, where n is the number of
# data examples, and p1 is the number of test features. For
# "supervised learning" methods, y may also be used for training.
#
# cvpar: 
#
# k: Number of folds.
#
# nc: Number of threads to use (passed as the mc.cores argument to
# mclapply).
#     
# VALUE
#
# The output is an m x k cross-validation matrix, where k is the
# number of folds and m is the number of parameter settings to
# evaluate (should be the same as the length of cvpar).
#
perform_cv <- function (fit, predict, evaluate, x, y, cvpar, k = 5, nc = 1) {

  # Get the number of data samples.
  n <- nrow(x)

  # Get the number of parameter settings.
  m <- length(cvpar)
  
  # Assign the data samples (rows of x, y) into k groups ("folds").
  folds <- sample(rep(1:k,length.out = n))

  # Set up the cross-validation matrix.
  out <- matrix(0,m,k)
  colnames(out) <- paste0("k",1:k)
  
  # Convert cvpar to a list if necessary.
  if (is.atomic(cvpar))
    out_rownames <- cvpar
  else
    out_rownames <- NULL
  if (!is.list(cvpar))
    cvpar <- as.list(cvpar)

  # Repeat the fit-predict-evaluate sequence for each fold and for
  # each parameter setting.
  configs <- expand.grid(list(fold = 1:k,param = 1:m))
  configs <- as.list(as.data.frame(t(configs)))
  f <- function (config)
    fit_predict_evaluate(x,y,folds,cvpar,fit,predict,evaluate,
                         config[1],config[2])
  if (nc == 1)
    out <- sapply(configs,f)
  else
    out <- unlist(mclapply(configs,f,mc.cores = nc))
  out <- matrix(out,m,k,byrow = TRUE)
  rownames(out) <- out_rownames
  colnames(out) <- paste0("k",1:k)
  
  # Return the cross-validation matrix.
  return(out)
}

# Implements the fit-predict-evaluate sequence for fold k and
# parameter setting cvpar[[i]].
fit_predict_evaluate <- function (x, y, folds, cvpar, fit, predict, evaluate,
                                  k, i) {
      
  # Generate the training and test sets.
  train <- which(folds != k)
  test  <- which(folds == k)    

  # Fit the model to the training data.
  model <- fit(x[train,],y[train,],cvpar[[i]])

  # Make predictions in the test samples.
  pred <- predict(x[test,],model)

  # Evaluate the predictions.
  return(evaluate(pred,y[test,]))
}
