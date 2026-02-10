# Update: 05/20/2024
# On vector observations
# Generate an independent validation set to tune the projection direction estimate
# t-test

SPT_ttest_msda_rwt_val = function(X1, X2, N1, N2, mu1, mu2, dsigma, sigma, lambda.list = NULL, 
                                  type=NULL, df=NULL, gammas1_true=NULL, gammas2_true=NULL, myseed){
  
  # Conduct t-test on projected sample
  n1 = nrow(X1); n2 = nrow(X2)
  n11 = N1[1]; n12 = N1[2]
  n21 = N2[1]; n22 = N2[2]
  p = ncol(X1)
  X11 = X1[1:n11,]; X12 = X1[(n11+1):n1,]
  X21 = X2[1:n21,]; X22 = X2[(n21+1):n2,]
  
  # reweighting
  if (!(is.null(gammas1_true) || is.null(gammas2_true))){
    obj11 = est_rose(X11, gammas.truth = gammas1_true[1:n11])
    obj21 = est_rose(X21, gammas.truth = gammas2_true[1:n21])
    Z11_ora = obj11$Xrwt_trueGamma; Z21_ora = obj21$Xrwt_trueGamma
    z1_cov_ora = obj11$sigma.oracle; z2_cov_ora = obj21$sigma.oracle
    z1_bar_ora = obj11$mu.oracle;    z2_bar_ora = obj21$mu.oracle
    sigma_hat_ora = (z1_cov_ora * (n11-1) + z2_cov_ora * (n21-1)) / (n11 + n21 - 2)
    z_bar_diff_ora = z1_bar_ora - z2_bar_ora
    
    obj12 = est_rose(X12, gammas.truth = gammas1_true[(n11+1):n1])
    obj22 = est_rose(X22, gammas.truth = gammas2_true[(n21+1):n2])
    # Z12_ora = obj12$Xrwt_trueGamma; Z22_ora = obj22$Xrwt_trueGamma
    mu_ora_overall = (obj12$mu.oracle + obj22$mu.oracle) / 2
    X12_ora_centered = X12 - t(matrix(rep(mu_ora_overall, n12), p, n12))
    X22_ora_centered = X22 - t(matrix(rep(mu_ora_overall, n22), p, n22))
    
    U12_ora = diag(1/gammas1_true[(n11+1):n1]) %*% X12_ora_centered
    V12_ora = diag(1/gammas2_true[(n21+1):n2]) %*% X22_ora_centered
  }
  else{
    obj11 = est_rose(X11); obj21 = est_rose(X21)
    obj12 = est_rose(X12); obj22 = est_rose(X22)
  }
  mu_est_overall = (obj12$mu.est + obj22$mu.est) / 2
  X12_centered = X12 - t(matrix(rep(mu_est_overall, n12), p, n12))
  X22_centered = X22 - t(matrix(rep(mu_est_overall, n22), p, n22))
  obj12_tilde = est_rose(X12_centered); obj22_tilde = est_rose(X22_centered)
  gammas1_tilde = obj12_tilde$gammas.est; gammas2_tilde = obj22_tilde$gammas.est
  U12 = diag(1 / gammas1_tilde) %*% X12_centered
  V12 = diag(1 / gammas2_tilde) %*% X22_centered
  # Z11 = obj11$Xrwt_estGamma; Z21 = obj21$Xrwt_estGamma
  # Z12 = obj12$Xrwt_estGamma; Z22 = obj22$Xrwt_estGamma

  # Estimate and tune the projection direction using an independent validation set
  z1_cov = obj11$sigma.est
  z1_bar = obj11$mu.est

  z2_cov = obj21$sigma.est
  z2_bar = obj21$mu.est
  
  sigma_hat = (z1_cov * (n11-1) + z2_cov * (n21-1)) / (n11 + n21 - 2)
  z_bar_diff = z1_bar - z2_bar
  
  # True prediction direction
  w_true = solve(sigma) %*% matrix(mu1 - mu2, ncol = 1) #atrans(mu1-mu2, lapply(sigma, solve))
  
  # Generate the validation set
  set.seed(myseed)
  objva_genX1 = gen_vecX(n11, p, mu1, dsigma, type = type, df = df)
  va_x1 = objva_genX1$X
  objva_genX2 = gen_vecX(n21, p, mu2, dsigma, type = type, df = df)
  va_x2 = objva_genX2$X
  
  obj_va_x1_rwt = est_rose(va_x1, gammas.truth = objva_genX1$gammas)
  obj_va_x2_rwt = est_rose(va_x2, gammas.truth = objva_genX2$gammas)

  Val_cov1 = obj_va_x1_rwt$sigma.est
  Val_x1_bar = obj_va_x1_rwt$mu.est
  Val_cov2 = obj_va_x2_rwt$sigma.est
  Val_x2_bar = obj_va_x2_rwt$mu.est
  Val_x_bar_diff = Val_x1_bar - Val_x2_bar
  sigma_vax = (Val_cov1 * (n11 - 1) + Val_cov2 * (n21 - 1)) / (n11 + n21 - 2)
  if (!(is.null(gammas1_true) || is.null(gammas2_true))){
    Val_cov1_ora = obj_va_x1_rwt$sigma.oracle
    Val_x1_bar_ora = obj_va_x1_rwt$mu.oracle
    Val_cov2_ora = obj_va_x2_rwt$sigma.oracle
    Val_x2_bar_ora = obj_va_x2_rwt$mu.oracle
    Val_x_bar_diff_ora = Val_x1_bar_ora - Val_x2_bar_ora
    sigma_val_ora = (Val_cov1_ora * (n11 - 1) + Val_cov2_ora * (n21 - 1)) / (n11 + n21 - 2)
  }
  
  if (is.null(lambda.list)){
    lambda.list = exp(seq(log(5), log(0.01), length=100))
  }
  
  fit = optm_msda(sigma_hat, z_bar_diff, n = min(n11,n21), lambda = lambda.list, pmax = p, dfmax = p, maxit = 10000)
  loss = rep(NA, length(fit$lambda))
  
  for (ilambda in 1:length(fit$lambda)){
    w_hat = matrix(fit$theta[[ilambda]], ncol = 1)
    if (sum(w_hat) == -1){
      break
    }else{
      loss[ilambda] = 1/2 * t(w_hat) %*% sigma_vax %*% w_hat - t(w_hat) %*% Val_x_bar_diff
      if (loss[ilambda]>1){
        break
      }
    }
  }
  
  nonzero = which(loss != 0)
  if (length(nonzero) > 0){
    loss = loss[nonzero]
    lambda.list_sel = fit$lambda[nonzero]
    
    if (min(loss)>0){
      print("Minimum loss greater than 0")
    }
    lambda.best = lambda.list_sel[which.min(loss)]
    lamidx = which(fit$lambda == lambda.best)
  }else{
    lambda.best = fit$lambda[length(fit$lambda)]
    lamidx = length(fit$lambda)
  }
  w_hat = matrix(fit$theta[[lamidx]], ncol = 1) # array(fit$theta[[lamidx]], dimen)
  if (sum(w_true^2)==0){
    dist = sqrt(sum((w_true - w_hat)^2))
  }else{
    dist = as.numeric(t(w_hat) %*% w_true / (norm(w_hat,"F") * norm(w_true, "F")))
  }
  
  # Projection and test
  y1 = U12 %*% w_hat
  y2 = V12 %*% w_hat

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

  #########################################################
  if (!(is.null(gammas1_true) || is.null(gammas2_true))){
    fit_ora = optm_msda(sigma_hat_ora, z_bar_diff_ora, n = min(n11,n21), lambda = lambda.list, pmax = p, dfmax = p, maxit = 10000)
    loss_ora = rep(NA, length(fit_ora$lambda))
    
    for (ilambda in 1:length(fit_ora$lambda)){
      w_hat_ora = matrix(fit_ora$theta[[ilambda]], ncol = 1)
      if (sum(w_hat_ora) == -1){
        break
      }else{
        loss_ora[ilambda] = 1/2 * t(w_hat_ora) %*% sigma_val_ora %*% w_hat_ora - t(w_hat_ora) %*% Val_x_bar_diff_ora
        if (loss_ora[ilambda]>1){
          break
        }
      }
    }
    
    nonzero = which(loss_ora != 0)
    if (length(nonzero) > 0){
      loss_ora = loss_ora[nonzero]
      lambda.list_ora_sel = fit_ora$lambda[nonzero]
      
      if (min(loss_ora)>0){
        print("Minimum loss greater than 0")
      }
      lambda.best_ora = lambda.list_ora_sel[which.min(loss_ora)]
      lamidx_ora = which(fit_ora$lambda == lambda.best_ora)
    }else{
      lambda.best_ora = fit_ora$lambda[length(fit_ora$lambda)]
      lamidx_ora = length(fit_ora$lambda)
    }
    w_hat_ora = matrix(fit_ora$theta[[lamidx_ora]], ncol = 1)
    if (sum(w_true^2)==0){
      dist_ora = sqrt(sum((w_true - w_hat_ora)^2))
    }else{
      dist_ora = as.numeric(t(w_hat_ora) %*% w_true / (norm(w_hat_ora,"F") * norm(w_true, "F")))
    }
    
    # Projection and test
    y1_ora = U12_ora %*% w_hat_ora
    y2_ora = V12_ora %*% w_hat_ora
    sy_pooled_ora = sqrt((var(y1_ora) * (n12-1) + var(y2_ora) *(n22-1)) / (n12+n22-2))
    if (sy_pooled_ora > 0){
      T_n_ora = (mean(y1_ora) - mean(y2_ora)) / (sy_pooled_ora * sqrt(1/n12 + 1/n22))
      # p_value = 2 * (1 - pnorm(abs(T_n), lower.tail  =  TRUE))
      p_value_ora = 2 * (1 - pt(abs(T_n_ora), df=n12+n22-2, lower.tail  =  TRUE))
    }else{
      T_n_ora = (mean(y1_ora) - mean(y2_ora)) / (sy_pooled_ora * sqrt(1/n12 + 1/n22))
      # p_value = 2 * (1 - pnorm(abs(T_n), lower.tail  =  TRUE))
      # p_value_ora = 1
      p_value_ora = 0.5
    }
  }
  else{
    p_value_ora = NULL
    T_n_ora = NULL
    w_hat_ora = NULL
    lambda.best_ora = NULL
    dist_ora = NULL
  }
  #########################################################
  
  
  return(list(p_value = p_value, T_n = T_n, p_value_ora = p_value_ora, T_n_ora = T_n_ora, 
              w_hat_vec = w_hat, 
              w_hat_vec_ora = w_hat_ora,
              dist = dist, dist_ora = dist_ora,
              lambda.best = lambda.best, lambda.best_ora = lambda.best_ora))
}