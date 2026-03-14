### check the skewness of the generated W with different alpha values

# Required package for skewness
library(moments)

# AR(0.5) covariance
make_AR_cov <- function(p, rho = 0.5) {
  outer(1:p, 1:p, function(i, j) rho^abs(i - j))
}

# Generate skew-normal W
generate_W_skew <- function(n, p, alpha = 3, rho = 0.5) {
  
  Sigma <- make_AR_cov(p, rho)
  chol_Sigma <- chol(Sigma)
  
  # Z1 ~ N(0, Sigma)
  Z1 <- matrix(rnorm(n * p), n, p) %*% chol_Sigma
  
  # Z2 ~ N(0, I)
  Z2 <- matrix(rnorm(n * p), n, p)
  
  # Raw skew-normal
  W_raw <- Z1 + alpha * abs(Z2)
  
  # Centering
  mean_shift <- alpha * sqrt(2/pi)
  W_centered <- sweep(W_raw, 2, mean_shift, "-")
  
  # Rescaling
  delta <- alpha^2 * (1 - 2/pi)
  W <- W_centered / sqrt(1 + delta)
  
  return(W)
}

set.seed(123)

n <- 5000
p <- 5
alpha <- 3

W <- generate_W_skew(n, p, alpha)

apply(W, 2, skewness)

for (a in c(0, 1, 2, 3, 5)) {
  W_temp <- generate_W_skew(5000, 1, alpha = a)
  cat("alpha =", a, "skewness =", skewness(W_temp), "\n")
}

hist(W[,1], breaks = 50, probability = TRUE,
     main = paste("Histogram of W (alpha =", alpha, ")"),
     col = "lightblue")

curve(dnorm(x, mean = 0, sd = 1),
      col = "red", lwd = 2, add = TRUE)

qqnorm(W[,1])
qqline(W[,1], col = "red", lwd = 2)

W_normal <- matrix(rnorm(n), n, 1)

par(mfrow=c(1,2))
hist(W_normal, main="Normal", col="gray")
hist(W[,1], main=paste("Skew alpha=",alpha), col="lightblue")

##############
### test random seed: below works, no need to use dorng
rm(list = ls())
library(doParallel)
library(parallel)
library(foreach)
closeAllConnections()
unregister_dopar()
n.cores <- parallel::detectCores()
retry({
  my.cluster <- parallel::makeCluster(n.cores, type = "FORK", outfile="")
  # my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK", outfile="") # use PSOCK for MacOS: On macOS, calling CoreFoundation / Objective-C from forked child processes is unsafe and triggers crash.
  doParallel::registerDoParallel(cl = my.cluster)
})
foreach::getDoParRegistered()
foreach::getDoParWorkers()

results <- foreach::foreach(i = 1:10, .combine = "rbind", .packages = c("highmean", "Hotelling")) %dopar% {
  
  set.seed(i)
  print(paste0("Iteration ", i, " with seed ", i))
  print(paste0("Random number: ", runif(1)))
  
  c(iteration = i, random_number = runif(1))
}
results

### test random seed: data generation is reproducible with the same seed in each iteration
rm(list = ls())

#### configuration ####
id.model <- 56
path.code <- paste0(getwd(), "/code/")
source(paste0(path.code, "config.R"))

library(doParallel)
library(parallel)
library(foreach)

closeAllConnections()
unregister_dopar()

n.cores <- parallel::detectCores()

my.cluster <- parallel::makeCluster(n.cores, type = "FORK", outfile="")
doParallel::registerDoParallel(my.cluster)

n_rep <- 100   # small test

#### function to generate data in parallel ####
run_sim <- function(){
  
  data_list <- foreach(i = 1:n_rep, .packages = c("highmean","Hotelling")) %dopar% {
    
    set.seed(i)
    
    data_all <- generate_data()
    
    return(data_all)
  }
  
  return(data_list)
}

#### run twice ####
data_run1 <- run_sim()
data_run2 <- run_sim()

#### check if identical ####
identical_results <- mapply(identical, data_run1, data_run2)

print(identical_results)
print(all(identical_results))

stopCluster(my.cluster)

################
### check if model_57_r1 and model_57_r2 has the same msda fit results
r1 <- readRDS(paste0(getwd(),"/output/raw/model_57_r1/iter_2.rds"))
r2 <- readRDS(paste0(getwd(),"/output/raw/model_57_r2/iter_2.rds"))
fit1 <- r1$fit.msda
fit2 <- r2$fit.msda
identical(fit1, fit2)
fit1.1 <- fit1[[1]]
fit2.1 <- fit2[[1]]
identical(fit1.1, fit2.1)
identical(fit1.1$lambda, fit2.1$lambda) # different number of successful lambda
fit1.1$lambda
fit2.1$lambda
len.lambda <- min(length(fit1.1$lambda), length(fit2.1$lambda))
len.lambda

### different number of successful lam is due to BLAS
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)
Sys.setenv(MKL_NUM_THREADS = 1)

### however for the same lambda, the theta can be different, and the non-zero pattern can be the same or different
all.equal(fit1.1$theta[[1]], fit2.1$theta[[1]])
all.equal(fit1.1$theta[[10]], fit2.1$theta[[10]])
all.equal(fit1.1$theta[[21]], fit2.1$theta[[21]])
all.equal(fit1.1$theta[[37]], fit2.1$theta[[37]])
ind <- 10
fit1.1$theta[[ind]][which(fit1.1$theta[[ind]]!=0)]
# compare above for 1:len.lambda
sapply(1:len.lambda, FUN = function(i) all.equal(fit1.1$theta[[i]], fit2.1$theta[[i]]))
# compare non-zero pattern
sapply(1:len.lambda, FUN = function(i) identical(which(fit1.1$theta[[i]]!=0), which(fit2.1$theta[[i]]!=0)))
# compare difference in objective value
sapply(1:len.lambda, FUN = function(i) fit1.1$obj[[i]] - fit2.1$obj[[i]])
# compare best lambda
which(r1$lamsel == r2$lamsel)
# index of lambda selected
M <- 40
sapply(1:M, FUN = function(i) which(fit1[[i]]$lambda == r1$lamsel[i]))
# print out npasses
sapply(1:len.lambda, FUN = function(i) c(npasses1 = fit1[[i]]$npasses, npasses2 = fit2[[i]]$npasses))
