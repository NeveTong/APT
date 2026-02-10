# Updated on 02/24/2023

optm_msda = function(sigma, delta, n, nlambda = 100, lambda.factor = ifelse((nobs - nclass) <= nvars, 0.2, 1e-03), 
                     lambda = NULL, dfmax = nobs, pmax = min(dfmax * 2 + 20, nvars), pf = rep(1, nvars), 
                     eps = 1e-04, maxit = 1e+06, sml = 1e-06, verbose = FALSE, perturb = NULL) {
  ## data setup
  this.call = match.call()
  # tmp = msda.prep(x, y)
  sigma = as.matrix(sigma)
  # if (!is.null(perturb)) 
    # diag(sigma) = diag(sigma) + perturb
  delta = as.matrix(t(delta)) # dim of delta should be 1*p!!!
  
  nobs = as.integer(n)  # maximum allowed parameter
  nvars = as.integer(dim(sigma)[2])
  nclass = as.integer(1)
  nk = as.integer(dim(delta)[1])

  ## parameter setup
  if (length(pf) != nvars) 
    stop("The size of penalty factor must be same as the number of input variables")
  maxit = as.integer(maxit)
  verbose = as.integer(verbose)
  sml = as.double(sml)
  pf = as.double(pf)
  eps = as.double(eps)
  dfmax = as.integer(dfmax)
  pmax = as.integer(pmax)
  vnames = paste("V", seq(nvars), sep = "")
  
  ## lambda setup
  nlam = as.integer(nlambda)
  if (is.null(lambda)) {
    if (lambda.factor >= 1) 
      stop("lambda.factor should be less than 1")
    flmin = as.double(lambda.factor)
    ulam = double(1)  #ulam=0 if lambda is missing
  } else {
    # flmin=1 if user define lambda
    flmin = as.double(1)
    if (any(lambda < 0)) 
      stop("lambdas should be non-negative")
    ulam = as.double(rev(sort(lambda)))  #lambda is declining
    nlam = as.integer(length(lambda))
  }
  ## call Fortran core
  fit = .Fortran("msda", obj = double(nlam), nk, nvars, as.double(sigma), as.double(delta), 
                  pf, dfmax, pmax, nlam, flmin, ulam, eps, maxit, sml, verbose, nalam = integer(1), 
                  theta = double(pmax * nk * nlam), itheta = integer(pmax), ntheta = integer(nlam), 
                  alam = double(nlam), npass = integer(1), jerr = integer(1))
  ## output: fit, maxit, pmax, nvars, vnames, nk
  outlist = formatoutput(fit, maxit, pmax, nvars, vnames, nk)
  outlist = c(outlist, list(npasses = fit$npass, jerr = fit$jerr, 
                             sigma = sigma, delta = delta, call = this.call))
  if (is.null(lambda)) 
    outlist$lambda = lamfix(outlist$lambda)
  class(outlist) = c("msda")
  outlist
}
