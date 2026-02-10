# paras include: 
# 1. distr.X: distribution of X
# 2. sigma: covariance matrix 
# 3. theta: 0 and 0.7 to evaluate the control of size and power, respectively
# 4. p: dimension
# 5. rho: correlation coefficient for AR(1) covariance structure

switch (id.model,
        # low dim
        "1" = { 
          id.distr.X <- "4"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        }
)

n1 = 40
n2 = 40
n_rep = 1000
theta <- switch (id.theta,
                 "1" = 0,
                 "2" = 0.7)
p <- switch (id.p,
             "1" = 100,
             "2" = 1000)
mu1 <- rep(0, p)
mu2 <- c(rnorm(n=10, mean=0, sd=theta^2), rep(0, p-10))
rho <- switch (id.rho,
               "1" = 0.1,
               "2" = 0.3,
               "3" = 0.5,
               "4" = 0.7)
switch (id.sigma,
        # equal covariance
        "1" = { 
          sigma = AR(rho, p)
          dsigma = t(chol(sigma))
        },
        # unequal covariance
        "2" = { # proportionally different covariance
          sigma1 = AR(rho, p)
          sigma2 = AR(rho, p) * 1.5
          dsigma1 = t(chol(sigma1))
          dsigma2 = t(chol(sigma2))
        },
        "3" = { # not proportionally different covariance
          sigma1 = AR(0.1, p)
          sigma2 = AR(0.7, p)
          dsigma1 = t(chol(sigma1))
          dsigma2 = t(chol(sigma2))
        })
