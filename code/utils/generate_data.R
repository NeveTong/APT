generate_data <- function() {
  switch(id.distr.X,
         "1" = { # normal
           obj_genX1 <- gen_vecX(n1, p, mu1, dsigma1, type = "Normal")
           obj_genX2 <- gen_vecX(n2, p, mu2, dsigma2, type = "Normal")
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
  
  return(list(obj_genX1 = obj_genX1,
              obj_genX2 = obj_genX2))
}