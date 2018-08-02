generate_data <- function(n, p){
  cov <- matrix(rnorm(n * p), nrow = n, ncol = p)
  resp <- rnorm(n)
  return(list(covariates = cov, 
              responses = resp))
}

model_select <- function(covariates, responses, cutoff = 0.05){
  lmodel <- lm(responses ~ covariates)
  coeffs <- summary(lmodel)$coefficients
  rows <- nrow(coeffs)
  cols <- ncol(coeffs)
  select <- coeffs[((rows * (cols - 1)) + 1):(rows * cols)] <= cutoff
  # -1 removes the constant term
  browser()
  newCov <- covariates[, select[-1]]
  if(ncol(newCov) == 0){
    return(vector())
  }
  refit <- lm(responses ~ newCov)
  return(summary(refit)$coefficients)
}
