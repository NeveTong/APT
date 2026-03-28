generate_data <- function() {
  switch(id.distr.X,
         "1" = { # normal
           obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "Normal", W_skew = W_skew)
           obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "Normal", W_skew = W_skew)
         },
         "2" = { # t6-distribution
           obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "T", df=6, W_skew = W_skew)
           obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "T", df=6, W_skew = W_skew)
         },
         "3" = { # t3-distribution
           obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "T", df=3, W_skew = W_skew)
           obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "T", df=3, W_skew = W_skew)
         },
         "4" = { # contaminated normal distribution CN1
           obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "Contaminated Normal1", W_skew = W_skew)
           obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "Contaminated Normal1", W_skew = W_skew)
         },
         "5" = { # contaminated normal distribution CN2
           obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "Contaminated Normal3", W_skew = W_skew)
           obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "Contaminated Normal3", W_skew = W_skew)
         },
         "6" = { # heterogeneous: X1 ~ t3 (unscaled), X2 ~ CN1 (rescaled to match t3 covariance)
           # t3 has E[gamma^2] = 3, so Cov(X1) = 3*Sigma
           # CN1 is rescaled so E[gamma^2] = 3 as well, giving Cov(X2) = 3*Sigma
           obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "T", df=3,
                                 W_skew = W_skew)
           obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "Contaminated Normal1",
                                 W_skew = W_skew, target_Egamma_sq = E_gamma_sq("T", 3))
         })
  
  return(list(obj_genX1 = obj_genX1,
              obj_genX2 = obj_genX2))
}