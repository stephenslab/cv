# TO DO: Explain briefly what this function does, and how to use it.
perform_cv <- function (fit, predict, evaluate, dat, cvpar, noncvpar = NULL,
                        k = 5, init = NULL) {

  # Get the number of data samples.
  n <- nrow(dat)

  # Get the number of parameter settings.
  m <- length(cvpar)
  
  # Assign the data samples (rows of dat) into k groups ("folds").
  folds <- sample(rep(1:k,length.out = n))

  # Set up the cross-validation matrix.
  out <- matrix(0,m,k)
  if (is.atomic(cvpar))
    rownames(out) <- cvpar
  colnames(out) <- paste0("k",1:k)
  
  # Convert cvpar to a list if necessary.
  if (!is.list(cvpar))
    cvpar <- as.list(cvpar)

  # Repeat for each fold.
  for (j in 1:k) {

    # Construct the training and test sets.
    train <- dat[folds != j,]
    test  <- dat[folds == j,]

    # Repeat for each parameter setting.
    for (i in 1:length(cvpar)) {

      # Fit the model to the training data.
      model <- fit(train,cvpar[[i]],noncvpar,init)

      # Make predictions in the test samples.
      pred <- predict(test,model)

      # Evaluate the predictions, and store the error measure in the
      # cross-validation matrix.
      out[i,j] <- evaluate(pred,test)
    }
  }
  
  # Return the cross-validation matrix.
  return(out)
}
