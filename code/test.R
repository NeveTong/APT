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
