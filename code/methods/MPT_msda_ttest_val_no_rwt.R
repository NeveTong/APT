# Update: 05/20/2024
# On vector observations
# Generate independent validation sets to tune the projection direction estimate
# t-test
# Depend on the SPT function in "SPT_msda_ttest_val.R"

MPT_ttest_msda_no_rwt_val = function(vec_X1, vec_X2, N1, N2, mu1, mu2, dsigma, sigma, 
                                 lambda.list = NULL, type=NULL, df=NULL,
                                 gammas1_true = NULL, gammas2_true = NULL,
                                 alpha=0.05, M=40){
  # M: the number of splittings
  
  n1 = nrow(vec_X1); n2 = nrow(vec_X2)
  n11 = N1[1]; n12 = N1[2]
  n21 = N2[1]; n22 = N2[2]
  nvars = ncol(vec_X1); p = ncol(vec_X1)
  dimen = dim(mu1)
  
  perms1 = matrix(0, M, n1); perms2 = matrix(0, M, n2)
  pvalues = rep(0, M)
  Tns = rep(0, M)
  zs = rep(0, M)
  dists = rep(NA, M)
  lamsel = rep(NA, M)
  
  for (mm in 1:M){
    # cat("The ", mm, "th split\n")
    set.seed(mm)
    perms1[mm,] = sample(n1, replace = FALSE)
    perms2[mm,] = sample(n2, replace = FALSE)
    tmp = SPT_ttest_msda_no_rwt_val(vec_X1[perms1[mm,],], vec_X2[perms2[mm,],], N1, N2, 
                                mu1, mu2, dsigma, sigma, 
                                lambda.list, type=type, df=df, myseed=mm+10000)
    pvalues[mm] = tmp$p_value
    Tns[mm] = tmp$T_n
    zs[mm] = qnorm(pvalues[mm])
    dists[mm] = tmp$dist
    lamsel[mm] = tmp$lambda.best
  }
  
  zs[zs == -Inf] = -10
  zs[zs == Inf] = 10
  zbar = mean(zs)
  zvar = var(zs)
  beta_cand = c(0.25, 0.25, 0.25, 0.25, 0.20, 0.20, 0.15, 0.15, 0.10, 0.05)
  cri_cand = c(1.988, 2.058, 2.133, 2.204, 2.489, 2.865, 3.126, 4.115, 7.17, 12.66)
  M_cand = c(2, 3, 4, 5, 10, 20, 40, 100, 1000, 10000)
  beta = beta_cand[which(M_cand == M)]
  
  rho1 = max(0, 1 - var(zs))
  # rho2 = max(0, 1-(M-1)*var(zs)/qchisq(1-beta, df=M-1, lower.tail = FALSE))
  rho2 = max(0, 1 - (M-1)*var(zs)/qchisq(1-beta, df=M-1, lower.tail = TRUE))
  
  cri_val1 = cri_cand[which(M_cand == M)]
  cri_val2 = qnorm(alpha/2, lower.tail = FALSE)
  
  Mrho1 = zbar / sqrt((1 + (M - 1) * rho1) / M)
  Mrho2 = zbar / sqrt((1 + (M - 1) * rho2) / M)
  
  rej1 = (abs(Mrho1) > cri_val1) * 1
  rej2 = (abs(Mrho2) > cri_val2) * 1
  
  return(list(pvalues=pvalues, zs=zs, rhos=c(rho1, rho2), cris=c(cri_val1,cri_val2), Tns = Tns,
              Mrhos = c(Mrho1, Mrho2), rejs = c(rej1, rej2), lamsel = lamsel, dists = dists,
              perms1 = perms1, perms2 = perms2))
}