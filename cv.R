# TO DO: Explain briefly what this function does, and how to use it.
perform_cv <- function (fit, predict, evaluate, dat, cvpar, noncvpar = NULL,
                        k = 5, nc = 1, init = NULL) {

  # Get the number of data samples.
  n <- nrow(dat)

  # Get the number of parameter settings.
  m <- length(cvpar)
  
  # Assign the data samples (rows of dat) into k groups ("folds").
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
    fit_predict_evaluate(dat,folds,cvpar,noncvpar,fit,predict,evaluate,init,
                         config[1],config[2])
  if (nc == 1)
    out <- sapply(configs,f)
  out <- matrix(out,m,k,byrow = TRUE)
  rownames(out) <- out_rownames
  colnames(out) <- paste0("k",1:k)
  
  # Convert cvpar to a list if necessary.
  if (is.atomic(cvpar))
    out_rownames <- cvpar
  
  # Return the cross-validation matrix.
  return(out)
}

# Implements the fit-predict-evaluate sequence for fold k and
# parameter setting i.
fit_predict_evaluate <- function (dat, folds, cvpar, noncvpar, fit, predict,
                                  evaluate, init, k, i) {
      
  # Construct the training and test sets.
  train <- dat[folds != k,]
  test  <- dat[folds == k,]

  # Fit the model to the training data.
  model <- fit(train,cvpar[[i]],noncvpar,init)

  # Make predictions in the test samples.
  pred <- predict(test,model)

  # Evaluate the predictions, and store the error measure in the
  # cross-validation matrix.
  return(evaluate(pred,test))
}
