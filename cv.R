# TO DO: Explain briefly what this function does, and how to use it.
perform_cv <- function (fit, evaluate, dat, cvpar, noncvpar = NULL, k = 5) {

  # Get the number of data samples.
  n <- nrow(dat)

  # Convert cvpar to a list if necessary.
  if (!is.list(cvpar))
    cvpar <- as.list(cvpar)
  
  # Assign the data samples (rows of dat) into k groups ("folds").
  folds <- sample(n,k)

  browser()
  
  # Return ...
  return(list())
}
