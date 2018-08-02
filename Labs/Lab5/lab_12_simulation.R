generate_data <- function(n, p){
  cov <- matrix(rnorm(n * p), nrow = n, ncol = p)
  resp <- rnorm(n)
  return(list(covariates = cov, 
              responses = resp))
}