generate_data <- function() {
  switch (id.sigma,
          "1" = { # equal covariance
            switch(id.distr.X,
                   "1" = { # normal
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma, type = "Normal")
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma, type = "Normal")
                   },
                   "2" = { # t6-distribution
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma, type = "T", df=6)
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma, type = "T", df=6)
                   },
                   "3" = { # t3-distribution
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma, type = "T", df=3)
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma, type = "T", df=3)
                   },
                   "4" = { # contaminated normal distribution CN1
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma, type = "Contaminated Normal1")
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma, type = "Contaminated Normal1")
                   },
                   "5" = { # contaminated normal distribution CN2
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma, type = "Contaminated Normal3")
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma, type = "Contaminated Normal3")
                   })
          },
          "2" = { # unequal covariance
            switch(id.distr.X,
                   "1" = { # normal
                     obj_genX1 <- matrix(rnorm(n1*p), n1, p) %*% t(dsigma1) + t(matrix(rep(mu1,n1),p,n1))
                     obj_genX2 <- matrix(rnorm(n2*p), n2, p) %*% t(dsigma2) + t(matrix(rep(mu2,n2),p,n2))
                   },
                   "2" = { # t6-distribution
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "T", df=6)
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "T", df=6)
                   },
                   "3" = { # t3-distribution
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "T", df=3)
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "T", df=3)
                   },
                   "4" = { # contaminated normal distribution CN1
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "Contaminated Normal1")
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "Contaminated Normal1")
                   },
                   "5" = { # contaminated normal distribution CN2
                     obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "Contaminated Normal3")
                     obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "Contaminated Normal3")
                   })
          })
  return(list(obj_genX1 = obj_genX1,
              obj_genX2 = obj_genX2))
}