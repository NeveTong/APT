# Update: 05/20/2024
# On vector observations
# Generate independent validation sets to tune the projection direction estimate
# t-test
# Depend on the SPT function in "SPT_msda_ttest_val.R"

MPT_ttest_msda_rwt_bic = function(vec_X1, vec_X2, N1, N2, mu1, mu2, sigma1, sigma2, 
                                   lambda.list = NULL, 
                                   gammas1_true = NULL, gammas2_true = NULL,
                                   alpha=0.05, M=40){
  # M: the number of splittings
  
  n1 = nrow(vec_X1); n2 = nrow(vec_X2)
  n11 = N1[1]; n12 = N1[2]
  n21 = N2[1]; n22 = N2[2]
  nvars = ncol(vec_X1); p = ncol(vec_X1)
  dimen = dim(mu1)
  
  perms1 = matrix(0, M, n1); perms2 = matrix(0, M, n2)
  pvalues = rep(0, M); pvalues_ora = rep(0, M)
  Tns = rep(0, M); Tns_ora = rep(0, M)
  zs = rep(0, M); zs_ora = rep(0, M)
  lamsel = rep(NA, M); lamsel_ora = rep(NA, M)
  dists = rep(NA, M); dists_ora = rep(NA, M)
  
  for (mm in 1:M){
    # cat("The ", mm, "th split\n")
    # set.seed(mm)
    perms1[mm,] = sample(n1, replace = FALSE)
    perms2[mm,] = sample(n2, replace = FALSE)
    tmp = SPT_ttest_msda_rwt_bic(vec_X1[perms1[mm,],], vec_X2[perms2[mm,],], N1, N2, 
                                  mu1, mu2, sigma1, sigma2, lambda.list, 
                                  gammas1_true = gammas1_true[perms1[mm,]], 
                                  gammas2_true = gammas2_true[perms2[mm,]])
    pvalues[mm] = tmp$p_value
    Tns[mm] = tmp$T_n
    zs[mm] = qnorm(pvalues[mm])
    lamsel[mm] = tmp$lambda.best
    dists[mm] = tmp$dist; dists_ora[mm] = tmp$dist_ora
    if (!is.null(gammas1_true)){
      pvalues_ora[mm] = tmp$p_value_ora
      Tns_ora[mm] = tmp$T_n_ora
      zs_ora[mm] = qnorm(pvalues_ora[mm])
      lamsel_ora[mm] = tmp$lambda.best_ora
    }
  }
  
  zs[zs == -Inf] = -10
  zs[zs == Inf] = 10
  zbar = mean(zs)
  zvar = var(zs)
  if (!is.null(gammas1_true)){
    zs_ora[zs_ora == -Inf] = -10
    zs_ora[zs_ora == Inf] = 10
    zbar_ora = mean(zs_ora)
    zvar_ora = var(zs_ora)
  }
  beta_cand = c(0.25, 0.25, 0.25, 0.25, 0.20, 0.20, 0.15, 0.15, 0.10, 0.05)
  cri_cand = c(1.988, 2.058, 2.133, 2.204, 2.489, 2.865, 3.126, 4.115, 7.17, 12.66)
  M_cand = c(2, 3, 4, 5, 10, 20, 40, 100, 1000, 10000)
  beta = beta_cand[which(M_cand == M)]
  
  rho1 = max(0, 1 - var(zs))
  rho2 = max(0, 1 - (M-1)*var(zs)/qchisq(1-beta, df=M-1, lower.tail = TRUE))
  if (!is.null(gammas1_true)){
    rho1_ora = max(0, 1 - var(zs_ora))
    rho2_ora = max(0, 1 - (M-1)*var(zs_ora)/qchisq(1-beta, df=M-1, lower.tail = TRUE))
  }else{
    rho1_ora = NULL; rho2_ora = NULL
  }
  
  cri_val1 = cri_cand[which(M_cand == M)]
  cri_val2 = qnorm(alpha/2, lower.tail = FALSE)
  
  Mrho1 = zbar / sqrt((1 + (M - 1) * rho1) / M)
  Mrho2 = zbar / sqrt((1 + (M - 1) * rho2) / M)
  if (!is.null(gammas1_true)){
    Mrho1_ora = zbar_ora / sqrt((1 + (M - 1) * rho1_ora) / M)
    Mrho2_ora = zbar_ora / sqrt((1 + (M - 1) * rho2_ora) / M)
  }
  
  rej1 = (abs(Mrho1) > cri_val1) * 1
  rej2 = (abs(Mrho2) > cri_val2) * 1
  if (!is.null(gammas1_true)){
    rej1_ora = (abs(Mrho1_ora) > cri_val1) * 1
    rej2_ora = (abs(Mrho2_ora) > cri_val2) * 1
  }
  
  return(list(pvalues=pvalues, zs=zs, pvalues_ora=pvalues_ora, zs_ora=zs_ora, 
              rhos=c(rho1, rho2), rhos_ora = c(rho1_ora, rho2_ora),
              cris=c(cri_val1,cri_val2), 
              Tns = Tns, Tns_ora = Tns_ora,
              Mrhos = c(Mrho1, Mrho2), Mrhos_ora = c(Mrho1_ora, Mrho2_ora),
              rejs = c(rej1, rej2), rejs_ora = c(rej1_ora, rej2_ora),
              lamsel = lamsel, lamsel_ora = lamsel_ora,
              dists = dists, dists_ora = dists_ora,
              perms1 = perms1, perms2 = perms2))
}