# paras include: 
# 1. distr.X: distribution of X
# 2. sigma: covariance matrix 
# 3. theta: 0 and 0.7 to evaluate the control of size and power, respectively
# 4. p: dimension
# 5. rho: correlation coefficient for AR(1) covariance structure

set.seed(2024)

switch (id.model,
        #### unequal covariance
        # proportionally different covariance
        "1" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "2" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "3" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "4" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "5" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "6" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "7" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "8" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "9" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "10" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "11" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "12" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "13" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "14" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "15" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "16" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "17" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "18" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "19" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "20" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "21" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "22" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "23" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "24" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "25" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "26" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "27" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "28" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "29" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "30" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"; id.W <- "1"; population_center <- F
        },
        "31" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "32" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "33" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "34" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "35" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"; id.W <- "1"; population_center <- F
        },
        "36" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "37" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "38" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "39" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "40" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        # not proportionally different covariance
        "41" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "42" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "43" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "44" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "45" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "46" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "47" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "48" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "49" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        "50" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "1"; population_center <- F
        },
        # skew normal W
        "51" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"; id.W <- "2"; population_center <- F
        },
        "52" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "2"; population_center <- F
        },
        "53" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"; id.W <- "2"; population_center <- F
        },
        "54" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "2"; population_center <- F
        },
        # centering the whole dataset with population mean
        "55" = { 
          id.distr.X <- "1"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "56" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "57" = { 
          id.distr.X <- "3"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "58" = { 
          id.distr.X <- "4"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "59" = { 
          id.distr.X <- "5"; id.sigma <- "1"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "60" = { 
          id.distr.X <- "1"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "61" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "62" = { 
          id.distr.X <- "3"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "63" = { 
          id.distr.X <- "4"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        "64" = { 
          id.distr.X <- "5"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- T
        },
        # original paper reproduction check
        "65" = { 
          id.distr.X <- "1"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "66" = { 
          id.distr.X <- "2"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "67" = { 
          id.distr.X <- "3"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "68" = { 
          id.distr.X <- "4"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
        },
        "69" = { 
          id.distr.X <- "5"; id.sigma <- "1"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"; id.W <- "1"; population_center <- F
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
mu2 <- c(rnorm(n=10, mean=0, sd=theta^2), rep(0, p-10)) # 0.298 -0.147 -1.063 -0.341 -0.676 -0.384 -0.483 -0.346 -0.108 0.743
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
