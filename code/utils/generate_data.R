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
         })
  
  return(list(obj_genX1 = obj_genX1,
              obj_genX2 = obj_genX2))
}