onesplit <- function(sam1, sam2,
                     ind1, ind2,
                     lambda0){
  k1 <- length(ind1); k2 <- length(ind2)
  sam11 <- sam1[ind1,]; sam12 <- sam1[-ind1,]
  sam21 <- sam2[ind2,]; sam22 <- sam2[-ind2,]
  sam1Y <- rbind(sam11, sam21)
  scalesam1Y <- rbind(scale(sam11, scale = FALSE),
                      scale(sam21, scale = FALSE))
  center <- sam1Y %*% t(scalesam1Y) / (k1+k2) + lambda0 * diag(k1+k2)
  r <- solve(center, sam1Y) %*%
    (colMeans(sam12) - colMeans(sam22))
  mean(r[1:k1]) - mean(r[(k1+1):(k1+k2)])
}

onesplit<-compiler::cmpfun(onesplit)

UProj <- function(sam1, sam2, B1 = 500, ratio = 0.9, lambda0 = NULL){
  n1 <- nrow(sam1)
  n2 <- nrow(sam2)
  n <- n1 + n2 - 2
  if (is.null(lambda0)) {
    lambda0 <- 1/sqrt(n)
  }
  k1 <- floor(ratio * n1)
  k2 <- floor(ratio * n2)
  f <- function(){
    ind1 <- sample(1:n1, k1)
    ind2 <- sample(1:n2, k2)
    onesplit(sam1, sam2, ind1, ind2, lambda0)
  }
  mean(replicate(B1, f()))
}

epval_UprojTwoSample <- function(sam1, sam2, B1 = 500,
                                 perm.iter = 500, ...){
  n1 <- nrow(sam1)
  n2 <- nrow(sam2)
  test.stat <- UProj(sam1 , sam2, B1 = B1)

  sam <- rbind(sam1, sam2)

  f <- function(){
    perm <- sample(1:(n1 + n2))
    sam1.perm <- sam[perm[1:n1], ]
    sam2.perm <- sam[perm[(n1 + 1):(n1 + n2)], ]
    UProj(sam1.perm, sam2.perm, B1 = B1)
  }

  test.stat.perm <- replicate(perm.iter, f())
  pval <- (sum(test.stat.perm >= test.stat) + 1)/(perm.iter + 1)
  names(pval) <- "Uproj"
  pval
}

