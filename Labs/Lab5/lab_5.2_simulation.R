generate_data <- function(n, p){
  cov <- matrix(rnorm(n * p), nrow = n, ncol = p)
  resp <- rnorm(n)
  return(list(covariates = cov, 
              responses = resp))
}

model_select <- function(covariates, responses, cutoff = 0.05){
  lmodel <- lm(responses ~ covariates)
  coeffs <- summary(lmodel)$coefficients
  select <- coeffs[,ncol(coeffs)] <= cutoff
  # -1 removes the constant term
  newCov <- matrix(covariates[, select[-1]], nrow = length(responses))
  if(ncol(newCov) == 0 | nrow(newCov) == 0){
    return(vector())
  }
  refit <- lm(responses ~ newCov)
  # -1 removes the constant term again
  return(summary(refit)$coefficients[,ncol(coeffs)][-1])
}

run_simulation <- function(n_trials, n, p, cutoff = 1){
  result = vector()
  for(i in 1:n_trials){
    data <- generate_data(n, p)
    result <- c(result, model_select(data$covariates, 
                                     data$responses, 
                                     cutoff = cutoff))
  }
  pdf(file = paste("Result", n, p, n_trials, ".pdf", sep = ""), 
      width = 10, height = 10)
  hist(result)
  graphics.off()
}

dir <- "/Users/Gabriel/Desktop/Notes\ Projects/Statistics/Statistical\ Computing/Labs/Lab5/Histograms"
setwd(dir)

for(i in 1:3){
  n_trials <- 1000
  n <- c(100, 1000, 10000)
  p <- c(10, 20, 50)
  run_simulation(n_trials = n_trials, n = n[i], p = p[i], cutoff = 0.05)
}