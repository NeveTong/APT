# paras include: 
# 1. distr.X: distribution of X
# 2. sigma: covariance matrix 
# 3. theta: 0 and 0.7 to evaluate the control of size and power, respectively
# 4. p: dimension
# 5. rho: correlation coefficient for AR(1) covariance structure

switch (id.model,
        #### unequal covariance
        # proportionally different covariance
        "1" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "2" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "3" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "4" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "5" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "6" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"
        },
        "7" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"
        },
        "8" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"
        },
        "9" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"
        },
        "10" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"
        },
        "11" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"
        },
        "12" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"
        },
        "13" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"
        },
        "14" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"
        },
        "15" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"
        },
        "16" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"
        },
        "17" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"
        },
        "18" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"
        },
        "19" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"
        },
        "20" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"
        },
        "21" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "22" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "23" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "24" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "25" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "26" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"
        },
        "27" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"
        },
        "28" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"
        },
        "29" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"
        },
        "30" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"
        },
        "31" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"
        },
        "32" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"
        },
        "33" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"
        },
        "34" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"
        },
        "35" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"
        },
        "36" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"
        },
        "37" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"
        },
        "38" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"
        },
        "39" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"
        },
        "40" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"
        },
        # not proportionally different covariance
        "41" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "42" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "43" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "44" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "45" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"
        },
        "46" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "47" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "48" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "49" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        "50" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"
        },
        # skew normal W
        "51" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "2"
        },
        "52" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "2"
        },
        "53" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "2"
        },
        "54" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "2"
        })

n1 = 40
n2 = 40
n_rep = 1000
theta <- switch (id.theta,
                 "1" = 0,
                 "2" = 0.7)
p <- switch (id.p,
             "1" = 1000,
             "2" = 100)
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
          sigma1 <- sigma2 <- AR(rho, p)
          dsigma1 <- dsigma2 <- t(chol(sigma1))
        },
        # unequal covariance
        "2" = { # proportionally different covariance
          sigma1 = AR(rho, p)
          sigma2 = AR(rho, p) * 1.5
          dsigma1 = t(chol(sigma1))
          dsigma2 = t(chol(sigma2))
        },
        "3" = { # not proportionally different covariance
          sigma1 = AR(0.3, p)
          sigma2 = AR(0.7, p)
          dsigma1 = t(chol(sigma1))
          dsigma2 = t(chol(sigma2))
        })
W_skew <- switch (id.W,
                 "1" = {W_skew <- F},
                 "2" = {W_skew <- T})
