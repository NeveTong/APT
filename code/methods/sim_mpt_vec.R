# Update: 05/24/2024
# t-test

sim = function(p, n1, n2, kap, sigma1, sigma2, mu1, mu2, obj_genX1, obj_genX2){

  n11 = n1 * kap; n12 = n1 - n11
  n21 = n2 * kap; n22 = n2 - n21
  N1 = c(n11, n12); N2 = c(n21, n22)
  
  # generate data
  # obj_genX1 = gen_vecX(n1, p, mu1, dsigma1, type, df)
  X1 = obj_genX1$X
  gammas1_true = obj_genX1$gammas
  W1 = obj_genX1$W
  
  # obj_genX2 = gen_vecX(n2, p, mu2, dsigma2, type, df)
  X2 = obj_genX2$X
  gammas2_true = obj_genX2$gammas
  W2 = obj_genX2$W

  # ===============================================
  #                   MPT
  # ===============================================
  # NEED TO CHECK IF WE NEED DIFFERENT SEQUENCES FOR ar(rho) and cs(rho)
  lambda.list = exp(seq(log(5), log(0.01), length=100))
  
  fit_mpt_msda_ttest_no_rwt_bic = MPT_ttest_msda_no_rwt_bic(X1, X2, N1, N2, mu1, mu2, sigma1, sigma2, lambda.list, 
                                                            gammas1_true=obj_genX1$gammas, gammas2_true=obj_genX2$gammas)
  rej_norwt_bic = fit_mpt_msda_ttest_no_rwt_bic$rejs
  lamsel_norwt_bic = fit_mpt_msda_ttest_no_rwt_bic$lamsel
  dists_norwt_bic = fit_mpt_msda_ttest_no_rwt_bic$dists
  
  fit_mpt_msda_ttest_rwt_bic = MPT_ttest_msda_rwt_bic(X1, X2, N1, N2, mu1, mu2, sigma1, sigma2, lambda.list,
                                                      gammas1_true = obj_genX1$gammas, gammas2_true = obj_genX2$gammas)
  rej_rwt_bic = fit_mpt_msda_ttest_rwt_bic$rejs
  rej_rwt_bic_ora = fit_mpt_msda_ttest_rwt_bic$rejs_ora
  lamsel_rwt_bic = fit_mpt_msda_ttest_rwt_bic$lamsel
  lamsel_rwt_bic_ora = fit_mpt_msda_ttest_rwt_bic$lamsel_ora
  dists_rwt_bic = fit_mpt_msda_ttest_rwt_bic$dists
  dists_rwt_bic_ora = fit_mpt_msda_ttest_rwt_bic$dists_ora
  
  res = list(rej_norwt = rej_norwt_bic,
             rej_rwt = rej_rwt_bic,
             rej_rwt_ora = rej_rwt_bic_ora,
             lamsel_norwt_bic = lamsel_norwt_bic,
             lamsel_rwt_bic = lamsel_rwt_bic, lamsel_rwt_bic_ora = lamsel_rwt_bic_ora,
             fit_mpt_msda_ttest_rwt_bic = fit_mpt_msda_ttest_rwt_bic,
             fit_mpt_msda_ttest_no_rwt_bic = fit_mpt_msda_ttest_no_rwt_bic,
             dists_norwt_bic = dists_norwt_bic,
             dists_rwt_bic = dists_rwt_bic,
             dists_rwt_bic_ora = dists_rwt_bic_ora)
  return(res)
}