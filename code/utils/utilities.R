# Update: 12/28/2023
# KKT check the output theta matrix
kktchk <- function(obj, pf, thr) {
  lambda <- obj$lambda
  nk <- nrow(obj$delta)
  nvars <- ncol(obj$sigma)
  for (l in 1:length(lambda)) {
    cat("now checking lambda ", l, "\n")
    theta <- t(obj$theta[[l]])
    thetaInner <- apply(theta, 2, crossprod)
    thetaNorm <- sqrt(thetaInner)
    
    sdiag <- diag(obj$sigma)
    sjj <- t(replicate(nk, sdiag))
    thetaTilda <- (obj$delta - theta %*% obj$sigma + sjj * theta)/sjj
    thetaDif <- theta - thetaTilda
    
    for (j in 1:nvars) {
      los <- lambda[l] * pf[j]/obj$sigma[j, j]
      if (thetaNorm[j] == 0) {
        dif_norm <- sqrt(crossprod(thetaDif[, j]))
        tmp <- dif_norm - los
        if (tmp > 0) 
          cat("violated at t > 0", tmp, "\n")
      } else {
        tmp3 <- thetaDif[, j] + los * theta[, j]/thetaNorm[j]
        if (any(abs(tmp3) > thr)) 
          cat("violated at t = 0", tmp3, "\n")
      }
    }
  }
}

# extract fortran outputs and format it into sparse matries
formatoutput <- function(fit, maxit, pmax, nvars, vnames, nk) {
  nalam <- fit$nalam
  ntheta <- fit$ntheta[seq(nalam)]
  nthetamax <- max(ntheta)
  lam <- fit$alam[seq(nalam)]
  obj <- fit$obj[seq(nalam)]
  stepnames <- paste("s", seq(nalam) - 1, sep = "")
  resnames <- paste("delta", seq(nk), sep = "")
  
  # self defined
  # nloops <- fit$jerr
  # if ((nloops < 0)&(nloops > -10000)){
  #   return(list(theta=-1))
  # } else{
  
  errmsg <- err(fit$jerr, maxit, pmax)  ### error messages from fortran
  switch(paste(errmsg$n), `1` = stop(errmsg$msg, call. = FALSE), `-1` = cat(errmsg$msg))
  
  dd <- c(nvars, nk)
  df <- rep(0, nalam)
  if (nthetamax > 0) {
    ja <- fit$itheta[seq(nthetamax)]
    oja <- order(ja)
    ja <- rep(ja[oja], nk)
    itheta <- cumsum(c(1, rep(nthetamax, nk)))
    pos <- rep(1:nalam, each = nk * pmax)
    theta <- split(fit$theta[seq(nk * pmax * nalam)], pos)
    for (l in 1:nalam) {
      theta[[l]] <- matrix(theta[[l]], pmax, nk, byrow = TRUE)[seq(nthetamax), 
                                                               , drop = FALSE]
      df[l] <- sum(rowSums(abs(theta[[l]])) != 0)
      theta[[l]] <- new("dgCMatrix", Dim = dd, Dimnames = list(vnames, resnames), 
                        x = as.vector(theta[[l]][oja, ]), p = as.integer(itheta - 
                                                                                                                             1), i = as.integer(ja - 1))
    }
  } else {
    theta <- list()
  if (nalam > 0){    
    for (l in 1:nalam) {
      theta[[l]] <- zeromat(nvars, nk, vnames, resnames)
    }
  }else{
      theta[[1]] <- zeromat(nvars, nk, vnames, resnames)
  }
    df <- rep(0, nalam)
  }
  list(theta = theta, df = df, dim = dd, lambda = lam, obj = obj)
  # }
}


# generate sigma, delta and mu from x, y
msda.prep <- function(x, y) {
  # data setup
  x <- as.matrix(x)
  y <- drop(y)
  nclass <- as.integer(length(unique(y)))
  prior <- rep(0, nclass)
  for (k in 1:nclass) {
    prior[k] <- mean(y == k)
  }
  nvars <- as.integer(ncol(x))
  nobs <- nrow(x)
  nres <- length(y)
  if (nres != nobs) 
    stop("x and y have different number of observations")
  # prepare sigma and delta
  mu <- matrix(0, nvars, nclass)
  sigma <- matrix(0, nvars, nvars)
  for (i in 1:nclass) {
    mu[, i] <- apply(x[y == i, ], 2, mean)
    sigma <- sigma + (sum(y == i) - 1) * cov(x[y == i, ])
  }
  sigma <- sigma/(nobs - nclass)
  delta <- mu[,2]-mu[,1]
  delta <- t(delta) # (nclass-1)*nvars
  outlist <- list(sigma = sigma, delta = delta, mu = mu, prior = prior)
  outlist
}

err <- function(n, maxit, pmax) {
  if (n == 0) 
    msg <- ""
  if (n > 0) {
    # fatal error
    if (n < 7777) 
      msg <- "Memory allocation error; contact package maintainer"
    if (n == 10000) 
      msg <- "All penalty factors are <= 0"
    n <- 1
    msg <- paste("in the fortran code -", msg)
  }
  if (n < 0) {
    # non fatal error
    if (n > -10000) 
      msg <- ""
      # msg <- paste("Convergence for ", -n, "th lambda value not reached after maxit=", 
      #              maxit, " iterations; solutions for larger lambdas returned.\n", 
      #              sep = "")
    if (n < -10000) 
      msg <- paste("Number of nonzero coefficients along the path exceeds pmax=", 
                   pmax, " at ", -n - 10000, "th lambda value; solutions for larger lambdas returned.\n", 
                   sep = "")
    if (n < -20000) 
      msg <- paste("Number of nonzero coefficients along the path exceeds dfmax=", 
                   pmax, " at ", -n - 20000, "th lambda value; solutions for larger lambdas returned.\n", 
                   sep = "")
    n <- -1
  }
  list(n = n, msg = msg)
}

zeromat <- function(nvars, nalam, vnames, stepnames) {
  ca <- rep(0, nalam)
  ia <- seq(nalam + 1)
  ja <- rep(1, nalam)
  dd <- c(nvars, nalam)
  new("dgCMatrix", Dim = dd, Dimnames = list(vnames, stepnames), x = as.vector(ca), 
      p = as.integer(ia - 1), i = as.integer(ja - 1))
}

lamfix <- function(lam) {
  llam <- log(lam)
  lam[1] <- exp(2 * llam[2] - llam[3])
  lam
}


SCAD_derivative <- function(beta, lambda, a=3.7) {
  diff=a*lambda-abs(beta)
  if (abs(beta)<=lambda){
    return(lambda)
  } else if (diff>0){
    return(diff/(a-1))
  } else {
    return(0)
  }
}








#######################################
# Covariance utility functions
#######################################
AR = function(rho, p){
  m = matrix(0, p, p)
  for (i in 1:p){
    for (j in 1:p){
      m[i, j] = rho ^ (abs(i - j))
    }
  }
  return(m)
}


CS = function(rho, p){
  m = matrix(rho, p, p)
  diag(m) = 1
  return(m)
}


AR_blk = function(rho, p, s){
  block = AR(rho,s)
  A = diag(rep(1,p/s),p/s,p/s)
  m = kronecker(A,block)
  return(m)
}


CS_blk = function(rho, p, s){
  block = CS(rho,s)
  A = diag(rep(1,p/s),p/s,p/s)
  m = kronecker(A,block)
  return(m)
}



SI = function(p){
  # Erdos-Renyi random graph
  vecu = rep(NA, p^2)
  vecdelta = rep(NA, p^2)
  for (i in seq(p^2)){
    vecdelta[i] = rbinom(1, 1, 0.05)
    tmp = rbinom(1, 1, 0.5)
    vecu[i] = tmp*runif(1, 0.5, 1)+(1-tmp)*runif(1, -1, -0.5)
  }
  omg = matrix(vecu * vecdelta, p, p)
  omg = (omg + t(omg)) / 2
  omg = omg + diag(p) * (max(0, -min(eigen(omg)$values)) + 0.05)
  w = matrix(0, p, p)
  diag(w) = 1 / sqrt(diag(omg))
  omg = w %*% omg %*% w
  sig = solve(omg)
  return(sig)
}


BS = function(p, s){
  # Block sparse precision
  B = diag(p)
  for (i in 1:s){
    for (j in (i+1):p){
      tmp = rbinom(1, 1, 0.3) * 0.5
      B[i,j] = tmp
      B[j,i] = tmp
    }
  }
  for (i in (s+1):(p-1)){
    for (j in (i+1):p){
      B[i,j] = 0.5
      B[j,i] = 0.5
    }
  }
  delta = max(0, -min(eigen(B)$values)) + 0.05
  Omega = (B + delta*diag(p)) / (1+delta)
  w = matrix(0, p, p)
  diag(w) = 1 / sqrt(diag(Omega))
  Omega = w %*% Omega %*% w
  sig = solve(Omega)
  return(sig)
}


TR = function(p){
  # mimics AR(1)
  sig = matrix(NA, p, p)
  hs = rep(NA, p)
  hs[1] = 0
  for (ii in 1:(p-1)){
    hs[ii+1] = hs[ii] + runif(1, 0.5, 1)
  }
  for (i in 1:p){
    for (j in 1:p){
      sig[i,j] = exp(-abs(hs[i]-hs[j])/2)
    }
  }
  return(sig)
}


# tensor frobineus norm
tnorm_F = function(x){
  f = sqrt(sum(x^2))
  return (f)
}






#######################################
#      Reweighting functions
#######################################
# Generate gammas
gen_gammas = function(n, type=NULL, df=NULL){
  # ------------------------------------------------------------
  # Input:
  #       - n: sample size; a scalar
  #       - type: what type of data you want to gamma to generate; 
  #               "Normal", "T", "Contaminated Normal1", "Contaminated Normal2", "Contaminated Normal3", or "Laplace"
  #       - df: degrees of freedom if you want to use gamma to generate t_df distributed data; 
  #             NULL or a scalar
  # Output: 
  #       - gammas: the generated gammas; a vector of length n
  # ------------------------------------------------------------
  
  if (is.null(type)){stop("Set a type for gamma!")}
  gammas = rep(NA, n)
  
  if (type == "Normal"){
    gammas = rep(1, n)
  }else if (type == "T"){
    tmp = rchisq(n, df=df, ncp=0)
    gammas = sqrt(df / tmp)
  }else if (type == "Contaminated Normal1"){
    # Pr(gamma = 5) = 0.1; Pr(gamma = 1) = 0.9
    tmp = runif(n, min=0, max=1)
    gammas[tmp <= 0.9] = 1
    gammas[tmp > 0.9] = 5
  }else if (type == "Contaminated Normal2"){
    # Pr(gamma = 3) = 0.1; Pr(gamma = 1) = 0.9
    tmp = runif(n, min=0, max=1)
    gammas[tmp <= 0.9] = 1
    gammas[tmp > 0.9] = 3
  }else if (type == "Contaminated Normal3"){
    # Pr(gamma = 10) = 0.1; Pr(gamma = 1) = 0.9
    tmp = runif(n, min=0, max=1)
    gammas[tmp <= 0.9] = 1
    gammas[tmp > 0.9] = 10
  }else if (type == "Laplace"){
    tmp = rexp(n, rate=1)
    gammas = sqrt(tmp)
  }else{
    stop("Select a type from 'T', 'Contaminated Normal1',
         'Contaminated Normal2', and 'Laplace'!")
  }
  return(gammas)
}

# Generate skew-normal W
generate_skew_W <- function(n, p, dsigma, alpha = 3) {
  # Generate Z1 ~ N(0, Sigma)
  Z1 <- matrix(rnorm(n * p), n, p) %*% t(dsigma)
  # Generate Z2 ~ N(0, I)
  Z2 <- matrix(rnorm(n * p), n, p)
  # Skew-normal construction
  W_raw <- Z1 + alpha * abs(Z2)
  
  # ---- Centering ----
  # E|Z| = sqrt(2/pi)
  mean_shift <- alpha * sqrt(2/pi)
  W_centered <- sweep(W_raw, 2, mean_shift, "-")
  
  # ---- Rescale variance ----
  # Var(|Z|) = 1 - 2/pi
  var_increase <- alpha^2 * (1 - 2/pi)
  
  # Marginal variance:
  # Var(Z1_j) = 1 (since AR diag = 1)
  marginal_var <- 1 + var_increase
  
  W <- W_centered / sqrt(marginal_var)
  
  return(W)
}


# Generate X
gen_vecX = function(n, p, mu, dsigma, type=NULL, df=NULL, W_skew = F){
  # ------------------------------------------------------------
  # Input:
  #       - n: sample size; a scalar
  #       - p: dimension of the data; a scalar
  #       - mu: mean of the data; a vector of length p
  #       - dsigma: the lower triangular matrix obtained from the Cholesky Decomposition of
  #                 covariance of the data; a p x p matrix
  #       - type: type of data; "Normal", "T", "Contaminated Normal1", "Contaminated Normal2", or "Laplace"
  #       - df: degrees of freedom if generating t_df distributed data; NULL or a scalar
  # Output: 
  #       - X: the generated data; a matrix of dimension n x p
  #            X[i,] = gammas[i] * W[i,]
  #       - gammas: the generated gammas; a vector of length n
  #       - W: the generated normal; a matrix of dimension n x p
  # ------------------------------------------------------------
  
  # Generate gammas
  gammas = gen_gammas(n, type=type, df=df)
  # Generate W's
  if (W_skew){
    W = generate_skew_W(n, p, dsigma)
  }else{
  W = matrix(rnorm(n*p), n, p)
  W = W %*% t(dsigma)
  }
  # Construct X
  X = matrix(NA, n, p)
  for (ii in 1:n){
    X[ii,] = mu + gammas[ii] * W[ii,]
  }
  
  return(list(X=X, gammas=gammas, W=W))
}


# Generate X
gen_tnsrX = function(n, dimen, mu, dsigma, type=NULL, df=NULL){
  # ------------------------------------------------------------
  # Input:
  #       - n: sample size; a scalar
  #       - dimen: dimension of the tensors; a vector of length M
  #       - mu: mean of the data; a tensor of dimension (p1,...,pM)
  #       - dsigma: a list of the lower triangular matrix obtained from the 
  #                 Cholesky Decomposition of the covariance along each mode
  #                 of the data; each element is a pm x pm matrix
  #       - type: type of data; "T", "Contaminated Normal1", "Contaminated Normal2", or "Laplace"
  #       - df: degrees of freedom if generating t_df distributed data; NULL or a scalar
  # Output: 
  #       - X: the generated data; a list of p1 x ... x pM tensors where
  #            X[[i]] = gammas[i] * W[[i]]
  #       - gammas: the generated gammas; a vector of length n
  #       - W: the generated normal; a list of p1 x ... x pM tensors
  # ------------------------------------------------------------

  # Generate gammas
  gammas = gen_gammas(n, type=type, df=df)
  # Generate W's
  W = array(rnorm(n*prod(dimen)), dim = c(dimen, n))
  W = rTensor::ttl(rTensor::as.tensor(W), dsigma, ms = seq(length(dimen)))
  W = asplit(W@data, length(dimen)+1)
  # Construct X
  X = list()
  for (ii in 1:n){
    X[[ii]] = mu + gammas[ii] * W[[ii]]
  }
  
  return(list(X=X, gammas=gammas, W=W))
}


# Robust estimation for vectors
est_rose = function(X, gammas.truth=NULL){
  # ------------------------------------------------------------
  # Input:
  #       - X: the data; a matrix of dimension n x p
  #            X[i,] = gammas[i] * W[i,]
  #       - gammas.truth: the true gammas; a vector of length n or NULL
  # Output: 
  #       - gammas.est: estimated gammas; a vector of length n
  #       - W.est: reweighted data; a matrix of dimension n x p
  #       - mu.est: estimated mean; a vector of length n
  #       - sigma.est: estimated covariance; a matrix of dimension p x p 
  #       - mu.oracle: estimated mean given true gammas; NA or a vector of length n
  #       - sigma.oracle: estimated covariance given true gammas; NA or a matrix of dimension p x p 
  #       - Xrwt_estGamma: reweighted data, which equals to W.est + mu.est; a matrix of dimension n x p
  #       - Xrwt_trueGamma: reweighted data, which equals to W.oracle + mu.est; a matrix of dimension n x p or NULL
  # ------------------------------------------------------------
  
  # X dimension: n x p
  dimen = dim(X)
  n = dimen[1]
  p = dimen[2]
  X.bar = colMeans(X)
  
  # Estimate gammas
  gammas.est = rep(NA, n)
  omegas.est = rep(NA, n)
  omegas.oracle = rep(NA, n)
  for (i in 1:n){
    gammas.est[i] = sqrt(sum((X[i,] - X.bar)^2) / p)
    omegas.est[i] = 1 / gammas.est[i]^2
    if (!is.null(gammas.truth)){
      omegas.oracle[i] = 1 / gammas.truth[i]^2
    }
  }
  
  # Estimate mu
  mu.est = rep(0, p)
  mu.oracle = rep(0, p)
  for (i in 1:n){
    mu.est = mu.est + omegas.est[i] * X[i,]
    if (!is.null(gammas.truth)){
      mu.oracle = mu.oracle + omegas.oracle[i] * X[i,]
    }
  }
  mu.est = mu.est / sum(omegas.est)
  if (!is.null(gammas.truth)){
    mu.oracle = mu.oracle / sum(omegas.oracle)
  }else{mu.oracle = NA}
  
  # Estimate W
  W.est = matrix(0, n, p)
  W.oracle = matrix(0, n, p)
  for (i in 1:n){
    W.est[i,] = (X[i,] - mu.est) / gammas.est[i]
    if (!is.null(gammas.truth)){
      W.oracle[i,] = (X[i,] - mu.oracle) / gammas.truth[i]
    }
  }
  if (is.null(gammas.truth)){W.oracle = NA}
  
  # Reweight X
  Xrwt_estGamma = matrix(0, n, p)
  Xrwt_trueGamma = matrix(0, n, p)
  for (i in 1:n){
    Xrwt_estGamma[i,] = X[i,] / gammas.est[i]
    if (!is.null(gammas.truth)){
      Xrwt_trueGamma[i,] = X[i,] / gammas.truth[i]
    }
  }
  if (is.null(gammas.truth)){Xrwt_trueGamma = NA}

 
  # Estimate Sigma
  W.bar = colMeans(W.est)
  W.bar_mat = t(matrix(rep(W.bar, n), p, n))
  sigma.est = t(W.est - W.bar_mat) %*% (W.est - W.bar_mat)
  sigma.est = sigma.est / n
  
  if (!is.null(gammas.truth)){
    Wora.bar = colMeans(W.oracle)
    Wora.bar_mat = t(matrix(rep(Wora.bar, n), p, n))
    sigma.oracle = t(W.oracle - Wora.bar_mat) %*% (W.oracle - Wora.bar_mat)
    sigma.oracle = sigma.oracle / n
  }else{sigma.oracle = NA}
  
  return(list(gammas.est=gammas.est, W.est=W.est,
              mu.est=mu.est, sigma.est=sigma.est,
              mu.oracle=mu.oracle, sigma.oracle=sigma.oracle,
              Xrwt_estGamma=Xrwt_estGamma, Xrwt_trueGamma=Xrwt_trueGamma))
}



# Robust estimation for tensors
est_rose_tnsr = function(X, gammas.truth=NULL){
  # ------------------------------------------------------------
  # Input:
  #       - X: the data; a list of p1 x ... x pM tensors with
  #            X[[i]] = gammas[i] * W[[i]]
  #       - gammas.truth: the true gammas; a vector of length n or NULL
  # Output: 
  #       - gammas.est: estimated gammas; a vector of length n
  #       - W.est: reweighted data; a matrix of dimension n x p
  #       - W.oracle: reweighted data with true gammas; a matrix of dimension n x p or NA
  #       - mu.est: estimated mean; a vector of length n
  #       - mu.oracle: estimated mean given true gammas; NA or a vector of length n
  #       - sigma.est: estimated covariance; a matrix of dimension p x p 
  #       - sigma.oracle: estimated covariance given true gammas; NA or a matrix of dimension p x p 
  #       - Xrwt_estGamma: reweighted data, which equals to W.est + mu.est; a matrix of dimension n x p
  #       - Xrwt_trueGamma: reweighted data, which equals to W.oracle + mu.est; a matrix of dimension n x p or NULL
  # ------------------------------------------------------------
  
  # X dimension: p1 x ... x pM
  dimen = dim(X[[1]])
  n = length(X)
  p = prod(dimen)
  Xmat = t(sapply(X, as.vector)) # n x p
  Xbar = colMeans(Xmat) # p x 1
  Xbar_mat = t(matrix(rep(Xbar, n), p, n)) # n x p
  
  # Estimate gammas
  gammas.est = sqrt(apply((Xmat - Xbar_mat)^2, 1, sum) / p)
  omegas.est = 1 / gammas.est^2
  if (!is.null(gammas.truth)){
    omegas.oracle = 1 / gammas.truth^2
  }else{omegas.oracle = NA}
  
  # Estimate mu
  mu.est = colSums(sweep(Xmat, 1, omegas.est, "*")) / sum(omegas.est)
  mu.est = array(mu.est, dimen) # p1 x ... x pM
  if (!is.null(gammas.truth)){
    mu.oracle = colSums(sweep(Xmat, 1, omegas.oracle, "*")) / sum(omegas.oracle)
    mu.oracle = array(mu.oracle, dimen)
  }else{mu.oracle = NA}
  
  # Estimate W
  W.est = list()
  W.oracle = list()
  for (i in 1:n){
    W.est[[i]] = (X[[i]] - mu.est) / gammas.est[i] # p1 x ... x pM
    if (!is.null(gammas.truth)){
      W.oracle[[i]] = (X[[i]] - mu.oracle) / gammas.truth[i] # p1 x ... x pM
    }
  }
  if (is.null(gammas.truth)){W.oracle = NA}
  
  # Reweight X
  Xrwt_estGamma = list()
  Xrwt_trueGamma = list()
  for (i in 1:n){
    Xrwt_estGamma[[i]] = X[[i]] / gammas.est[i]
    if (!is.null(gammas.truth)){
      Xrwt_trueGamma[[i]] = X[[i]] / gammas.truth[i]
    }
  }
  if (is.null(gammas.truth)){Xrwt_trueGamma = NA}
  
  
  # Estimate Sigma
  obj.est = prept_mean_cov(W.est)
  sigma.est = obj.est$sig_est #sigma.est / n
  
  if (!is.null(gammas.truth)){
    obj.oracle = prept_mean_cov(W.oracle)
    sigma.oracle = obj.oracle$sig_est #sigma.oracle / n
  }else{sigma.oracle = NA}
  
  return(list(gammas.est=gammas.est, 
              W.est=W.est, W.oracle=W.oracle,
              mu.est=mu.est, sigma.est=sigma.est,
              mu.oracle=mu.oracle, sigma.oracle=sigma.oracle,
              Xrwt_estGamma=Xrwt_estGamma, Xrwt_trueGamma=Xrwt_trueGamma))
}




prept_mean_cov = function(x) {
  # ------------------------------------------------------------
  # Input:
  #       - x: random tensors; a list of tensors of dimension p1 x p2 x ... x pM
  # Output: MOM covariance estimates
  #       - mu_est: mean estimate; a tensor of size p1 x p2 x ... x pM
  #       - sig_est: scaled covariance estimates; a list of matrices of length M with each element of dimension pm x pm
  # ------------------------------------------------------------
  
  nobs = length(x)
  dimen = dim(x[[1]])
  ldim = length(dimen) 
  nvars = prod(dimen)
  nvars_m = rep(NA, ldim)
  for (m in 1:ldim){
    nvars_m[m] = nvars / dimen[m]
  }
  
  # Mean estimate
  mu_est = apply(abind::abind(x, along = ldim+1), seq(ldim), mean)
  
  # Covariance estimate -- MOM
  sig_est = array(list(), ldim)
  for (m in 1:ldim){
    sig_est[[m]] = matrix(0, dimen[m], dimen[m])
    for (i in 1:nobs){
      xi_adj = x[[i]] - mu_est
      sig_est[[m]] = sig_est[[m]] + mat(xi_adj, m) %*% t(mat(xi_adj, m))
    }
    sig_est[[m]] = sig_est[[m]] / (nobs * nvars_m[m])
  }
  aa = 1
  for (m in 1:ldim){
    if (m < ldim){
      aa = aa * sig_est[[m]][1,1]
      sig_est[[m]] = sig_est[[m]] / sig_est[[m]][1,1]
    }else{
      sig_est[[m]] = sig_est[[m]] * aa 
    }
  }
  
  maxd = max(dimen)
  sigma = matrix(0, nrow = ldim, ncol = (maxd^2))
  for (i in 1:ldim){
    tmp = matrix(0, nrow = maxd, ncol = maxd)
    tmp[1:dimen[i], 1:dimen[i]] = sig_est[[i]]
    sigma[i,] = as.vector(tmp)
  }
  outlist = list(sigma = sigma, mu_est = mu_est, sig_est = sig_est)
  outlist
}



pooled_cov = function(x, y) {
  # ------------------------------------------------------------
  # Input:
  #       - x: random tensors; a list of tensors of dimension p1 x p2 x ... x pM
  #       - y: random tensors; a list of tensors of dimension p1 x p2 x ... x pM
  # Output: MOM covariance estimates
  #       - mu_est1: mean estimate of x; a tensor of size p1 x p2 x ... x pM
  #       - mu_est2: mean estimate of y; a tensor of size p1 x p2 x ... x pM
  #       - sig_est: scaled covariance estimates; a list of matrices of length M with each element of dimension pm x pm
  #       - sig_est_raw: covariance estimates; a list of matrices of length M with each element of dimension pm x pm
  # ------------------------------------------------------------
  
  nobs1 = length(x); nobs2 = length(y)
  dimen = dim(x[[1]])
  ldim = length(dimen) 
  nvars = prod(dimen)
  nvars_m = rep(NA, ldim)
  for (m in 1:ldim){
    nvars_m[m] = nvars / dimen[m]
  }
  
  # Mean estimate
  mu_est1 = apply(abind::abind(x, along = ldim+1), seq(ldim), mean)
  mu_est2 = apply(abind::abind(y, along = ldim+1), seq(ldim), mean)
  
  # Covariance estimate -- MOM
  sig_est = array(list(), ldim)

  for (m in 1:ldim){
    sig_est[[m]] = matrix(0, dimen[m], dimen[m])
    
    for (i in 1:nobs1){
      xi_adj = x[[i]] - mu_est1
      sig_est[[m]] = sig_est[[m]] + mat(xi_adj, m) %*% t(mat(xi_adj, m))
    }
    
    for (i in 1:nobs2){
      yi_adj = y[[i]] - mu_est2
      sig_est[[m]] = sig_est[[m]] + mat(yi_adj, m) %*% t(mat(yi_adj, m))
    }
    
    sig_est[[m]] = sig_est[[m]] / ((nobs1 + nobs2 - 2) * nvars_m[m])
  }
  
  aa = 1
  for (m in 1:ldim){
    if (m < ldim){
      aa = aa * sig_est[[m]][1,1]
      sig_est[[m]] = sig_est[[m]] / sig_est[[m]][1,1]
    }else{
      sig_est[[m]] = sig_est[[m]] * aa 
    }
  }
  
  maxd = max(dimen)
  sigma = matrix(0, nrow = ldim, ncol = (maxd^2))
  for (i in 1:ldim){
    tmp = matrix(0, nrow = maxd, ncol = maxd)
    tmp[1:dimen[i], 1:dimen[i]] = sig_est[[i]]
    sigma[i,] = as.vector(tmp)
  }
  outlist = list(sigma = sigma, mu_est1 = mu_est1, mu_est2 = mu_est2, sig_est = sig_est)
  outlist
}
