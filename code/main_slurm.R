rm(list = ls())

#### read in simulation settings ####
r.args <- commandArgs(trailingOnly=T)
print(r.args)
id.model <- r.args[1]

print(paste0("Start Simulating Model ", id.model))

#### configuration ####
path.code <- paste0(getwd(), "/code/")
source(paste0(path.code, "config.R"))

# #### parallel computing setup - MacOS ####
# library(doParallel)
# library(parallel)
# library(foreach)
# closeAllConnections()
# unregister_dopar()
# n.cores <- parallel::detectCores()
# retry({
#   my.cluster <- parallel::makeCluster(n.cores, type = "FORK", outfile="")
#   # my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK", outfile="") # use PSOCK for MacOS: On macOS, calling CoreFoundation / Objective-C from forked child processes is unsafe and triggers crash.
#   doParallel::registerDoParallel(cl = my.cluster)
# })
# foreach::getDoParRegistered()
# foreach::getDoParWorkers()

#### parallel computing setup - Slurm ####
library(doMPI)
library(foreach)
cl <- startMPIcluster()
registerDoMPI(cl)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

print(system.time({
  results <- foreach::foreach(i = 1:n_rep, .combine = "rbind", .packages = c("highmean", "Hotelling")) %dopar% {
    
    set.seed(i)
    
    data_all <- generate_data()
    obj_genX1 <- data_all$obj_genX1
    obj_genX2 <- data_all$obj_genX2
    Y1 <- obj_genX1$X
    Y2 <- obj_genX2$X
    
    #### Bai and Saranadasa (1996)
    epval_BS <- NA
    tryCatch({
      epval_BS = highmean::epval_Bai1996(Y1, Y2, n.iter=200)$pval
    }, error = function(e) {
      message(paste0("Error in BS, iteration ", i, ": ", e))
    })
    
    ### Xu G, Lin L, Wei P, and Pan W (2016)
    epval_XLWP <- NA
    tryCatch({
      epval_XLWP = highmean::epval_aSPU(Y1, Y2, n.iter=200)$pval[8]
    }, error = function(e) {
      message(paste0("Error in XLWP, iteration ", i, ": ", e))
    })
    
    ### Cai, Liu, and Xia (2014)
    epval_CLX <- NA
    tryCatch({
      epval_CLX = highmean::epval_Cai2014(Y1, Y2, n.iter=200)$pval
    }, error = function(e) {
      message(paste0("Error in CLX, iteration ", i, ": ", e))
    })
    
    ### Chen and Qin (2010)
    epval_CQ <- NA
    tryCatch({
      epval_CQ = highmean::epval_Chen2010(Y1, Y2, n.iter=200)$pval
    }, error = function(e) {
      message(paste0("Error in CQ, iteration ", i, ": ", e))
    })
    
    ### Chen, Li, and Zhong (2014/2019)
    epval_CLZ <- NA
    tryCatch({
      epval_CLZ = highmean::epval_Chen2014(Y1, Y2, n.iter=200)$pval
    }, error = function(e) {
      message(paste0("Error in CLZ, iteration ", i, ": ", e))
    })
    
    ### Srivastava and Du (2008)
    epval_SD <- NA
    tryCatch({
      epval_SD = highmean::epval_Sri2008(Y1, Y2, n.iter=200)$pval
    }, error = function(e) {
      message(paste0("Error in SD, iteration ", i, ": ", e))
    })
    
    #### Lopes, Jacob, and Wainwright (2011)
    aRPT_pval <- NA
    n = n1 + n2 - 2
    P = matrix(rnorm(p * floor(n/2)), p, floor(n/2))
    Z1 = Y1 %*% P
    Z2 = Y2 %*% P
    tryCatch({
      test = Hotelling::hotelling.test(x=Z1, y=Z2)
      aRPT_pval = test$pval
    }, error = function(e) {
      message(paste0("Error in RPT, iteration ", i, ": ", e))
    })
    
    #### Li and Li (2021)
    LL_pval <- NA
    tryCatch({
      LL_pval = epval_UprojTwoSample(Y1, Y2, B1 = 500, perm.iter = 200)
    }, error = function(e) {
      message(paste0("Error in LL, iteration ", i, ": ", e))
    })
    
    #### PT and APT
    PT_rej <- NA
    APT_rej <- NA
    tryCatch({
      kap = 1/2
      PT_APT = sim(p, n1, n2, kap, sigma1, sigma2, mu1, mu2, obj_genX1, obj_genX2)
      PT_rej = PT_APT$rej_norwt[2]
      APT_rej = PT_APT$rej_rwt[2]
    }, error = function(e) {
      message(paste0("Error in PT, iteration ", i, ": ", e))
    })
    
    return(c(epval_BS, epval_CQ, epval_SD, epval_CLZ, epval_CLX, epval_XLWP, aRPT_pval, LL_pval, PT_rej, APT_rej))                            
  }
}))

colnames(results) <- c("BS", "CQ", "SD", "CLZ", "CLX", "XLWP", "RPT", "LL", "PT", "APT")
saveRDS(results, file = paste0(path.output, "raw/model_", id.model, ".rds"))

print(paste0("Model ", id.model))
rejs <- colMeans(results[,1:8] < 0.05, na.rm = TRUE)
rejs_all <- c(rejs, colMeans(results[,9:10], na.rm = TRUE))
print(rejs_all)
saveRDS(rejs_all, file = paste0(path.output, "/metric/model_", id.model, ".rds"))

# #### stop cluster - MacOS ####
# closeAllConnections()
# closeAllConnections()
# library(beepr)
# beep(2)

#### stop cluster - Slurm ####
closeCluster(cl)
mpi.quit()

# beep(2)
