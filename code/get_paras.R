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
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "2" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "3" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "4" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "5" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "6" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "7" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "8" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "9" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "10" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "11" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "12" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "13" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "14" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "15" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "16" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "17" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "18" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "19" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "20" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "21" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "22" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "23" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "24" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "25" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "26" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "27" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "28" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "29" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "30" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "31" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "32" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "33" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "34" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "35" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "36" = { 
          id.distr.X <- "1"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "37" = { 
          id.distr.X <- "2"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "38" = { 
          id.distr.X <- "3"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "39" = { 
          id.distr.X <- "4"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "40" = { 
          id.distr.X <- "5"; id.sigma <- "2"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        # not proportionally different covariance
        "41" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "42" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "43" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "44" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "45" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "1"
        },
        "46" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "47" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "48" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "49" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "50" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "2"
        },
        "51" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "52" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "53" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "54" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "55" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "3"
        },
        "56" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "57" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "58" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "59" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "60" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "1"; id.rho <- "4"
        },
        "61" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "62" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "63" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "64" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "65" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "1"
        },
        "66" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "67" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "68" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "69" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "70" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "2"
        },
        "71" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "72" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "73" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "74" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "75" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "3"
        },
        "76" = { 
          id.distr.X <- "1"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "77" = { 
          id.distr.X <- "2"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "78" = { 
          id.distr.X <- "3"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "79" = { 
          id.distr.X <- "4"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
        },
        "80" = { 
          id.distr.X <- "5"; id.sigma <- "3"; id.p <- "1"; id.theta <- "2"; id.rho <- "4"
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
          sigma1 <- sigma2 <- AR(rho, p)
          dsigma1 <- dsigma2 <- t(chol(sigma))
        },
        # unequal covariance
        "2" = { # proportionally different covariance
          sigma1 = AR(rho, p)
          sigma2 = AR(rho, p) * 1.5
          dsigma1 = t(chol(sigma1))
          dsigma2 = t(chol(sigma2))
        },
        "3" = { # not proportionally different covariance
          sigma1 = AR(0.5, p)
          sigma2 = AR(0.7, p)
          dsigma1 = t(chol(sigma1))
          dsigma2 = t(chol(sigma2))
        })
