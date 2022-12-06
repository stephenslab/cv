# TO DO: Explain briefly what this function does, and how to use it.
perform_cv <- function (fit, predict, evaluate, x, y, cvpar, noncvpar = NULL,
                        k = 5, nc = 1, init = NULL) {

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
    fit_predict_evaluate(x,y,folds,cvpar,noncvpar,fit,predict,evaluate,init,
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
fit_predict_evaluate <- function (x, y, folds, cvpar, noncvpar, fit,
                                  predict, evaluate, init, k, i) {
      
  # Generate the training and test sets.
  train <- which(folds != k)
  test  <- which(folds == k)    

  # Fit the model to the training data.
  model <- fit(x[train,],y[train,],cvpar[[i]],noncvpar,init)

  # Make predictions in the test samples.
  pred <- predict(x[test,],model)

  # Evaluate the predictions.
  return(evaluate(pred,y[test,]))
}
