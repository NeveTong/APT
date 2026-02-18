# Update: 05/24/2024
# On vector observations
# Generate an independent validation set to tune the projection direction estimate
# t-test

SPT_ttest_msda_no_rwt_bic = function(X1, X2, N1, N2, mu1, mu2, sigma1, sigma2, 
                                     lambda.list = NULL){
  
  # Conduct t-test on projected sample
  n1 = nrow(X1); n2 = nrow(X2)
  n11 = N1[1]; n12 = N1[2]
  n21 = N2[1]; n22 = N2[2]
  p = ncol(X1)
  X11 = X1[1:n11,]; X12 = X1[(n11+1):n1,]
  X21 = X2[1:n21,]; X22 = X2[(n21+1):n2,]
  
  # reweighting -- no reweighting
  Z11 = X11; Z21 = X21
  Z12 = X12; Z22 = X22

  # Estimate and tune the projection direction using an independent validation set
  z1_cov = cov(Z11)
  z1_bar = colMeans(Z11)

  z2_cov = cov(Z21)
  z2_bar = colMeans(Z21)
  
  sigma_hat = (z1_cov * (n11-1) + z2_cov * (n21-1)) / (n11 + n21 - 2)
  z_bar_diff = z1_bar - z2_bar
  
  # True prediction direction
  w_true = solve(sigma1) %*% matrix(mu1 - mu2, ncol = 1)
  
  if (is.null(lambda.list)){
    lambda.list = exp(seq(log(5), log(0.01), length=100))
  }
  
  fit = optm_msda(sigma_hat, z_bar_diff, n = min(n11,n21), lambda = lambda.list, pmax = p, dfmax = p, maxit = 10000)
  BICs = rep(NA, length(fit$lambda))
  
  for (ilambda in 1:length(fit$lambda)){
    w_hat = matrix(fit$theta[[ilambda]], ncol = 1)
    if (sum(w_hat) == -1){
      break
    }else{
      BICs[ilambda] = norm(matrix(z_bar_diff, ncol=1) - sigma_hat %*% w_hat,"F")^2 + sum(w_hat == 0) * log(log(n12+n22)) * log(p) / (n12+n22)
    }
  }
  
  lambda.best = fit$lambda[which.min(BICs)]
  lamidx = which(fit$lambda == lambda.best)
  # nonzero = which(BICs != sum(z_bar_diff^2))
  # if (length(nonzero) > 0){
  #   BICs = BICs[nonzero]
  #   lambda.list_sel = fit$lambda[nonzero]
  #   
  #   lambda.best = lambda.list_sel[which.min(BICs)]
  #   lamidx = which(fit$lambda == lambda.best)
  # }else{
  #   lambda.best = fit$lambda[length(fit$lambda)]
  #   lamidx = length(fit$lambda)
  # }
  
  w_hat = matrix(fit$theta[[lamidx]], ncol = 1)
  if (sum(w_true^2)==0){
    dist = sqrt(sum((w_true - w_hat)^2))
  }else{
    dist = as.numeric(t(w_hat) %*% w_true / (norm(w_hat,"F") * norm(w_true, "F")))
  }
  
  # Projection and test
  y1 = Z12 %*% w_hat
  y2 = Z22 %*% w_hat
  
  if (identical(sigma1, sigma2)){
    sy_pooled = sqrt((var(y1) * (n12-1) + var(y2) *(n22-1)) / (n12+n22-2))
    if (sy_pooled > 0){
      T_n = (mean(y1) - mean(y2)) / (sy_pooled * sqrt(1/n12 + 1/n22))
      # p_value = 2 * (1 - pnorm(abs(T_n), lower.tail  =  TRUE))
      p_value = 2 * (1 - pt(abs(T_n), df=n12+n22-2, lower.tail  =  TRUE))
    }else{
      T_n = (mean(y1) - mean(y2)) / (sy_pooled * sqrt(1/n12 + 1/n22))
      # p_value = 2 * (1 - pnorm(abs(T_n), lower.tail  =  TRUE))
      # p_value = 1
      p_value = 0.5
    }
  } else{ # Welch's t-test
    var_y1 = var(y1)
    var_y2 = var(y2)
    denominator = sqrt(var_y1/n12 + var_y2/n22)
    if (denominator == 0){
      T_n = Inf
      p_value = 0.5
    }else{
      T_n = (mean(y1) - mean(y2)) / denominator
      df_num = (var_y1/n12 + var_y2/n22)^2
      df_denom = (var_y1^2 / (n12^2 * (n12-1))) + (var_y2^2 / (n22^2 * (n22-1)))
      df = df_num / df_denom
      # p_value = 2 * (1 - pnorm(abs(T_n), lower.tail  =  TRUE))
      p_value = 2 * (1 - pt(abs(T_n), df=df, lower.tail  =  TRUE))
    }
  }
  

  
  #########################################################
  
  
  return(list(p_value = p_value, T_n = T_n, lams = fit$lambda,
              w_hat = w_hat, dist = dist, BICs = BICs,
              lambda.best = lambda.best))
}