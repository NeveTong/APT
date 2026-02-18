rm(list = ls())

#### read in simulation settings ####
r.args <- commandArgs(trailingOnly = TRUE)
print(r.args)
id.model <- r.args[1]

for (ind in c(1:5)) {
  
  id.model <- as.character(ind)
  print(paste0("Start Simulating Model ", id.model))
  
  #### configuration ####
  path.code <- paste0(getwd(), "/code/")
  source(paste0(path.code, "config.R"))
  
  #### parallel computing setup - MacOS ####
  library(doParallel)
  library(parallel)
  library(foreach)
  
  closeAllConnections()
  unregister_dopar()
  
  n.cores <- parallel::detectCores()
  
  retry({
    my.cluster <- parallel::makeCluster(n.cores, type = "FORK", outfile = "")
    doParallel::registerDoParallel(cl = my.cluster)
  })
  
  foreach::getDoParRegistered()
  foreach::getDoParWorkers()
  
  print(system.time({
    
    results <- foreach::foreach(
      i = 1:n_rep,
      .combine = "rbind"
    ) %dopar% {
      
      set.seed(i)
      
      data_all <- generate_data()
      obj_genX1 <- data_all$obj_genX1
      obj_genX2 <- data_all$obj_genX2
      Y1 <- obj_genX1$X
      Y2 <- obj_genX2$X
      
      #### PT and APT only ####
      PT_rej <- NA
      APT_rej <- NA
      
      tryCatch({
        kap = 1/2
        PT_APT = sim(p, n1, n2, kap,
                     sigma1, sigma2,
                     mu1, mu2,
                     obj_genX1, obj_genX2)
        
        PT_rej  = PT_APT$rej_norwt[2]
        APT_rej = PT_APT$rej_rwt[2]
        
      }, error = function(e) {
        message(paste0("Error in PT/APT, iteration ", i, ": ", e))
      })
      
      return(c(PT_rej, APT_rej))
    }
    
  }))
  
  colnames(results) <- c("PT", "APT")
  
  saveRDS(results,
          file = paste0(path.output, "raw/model_", id.model, "_APT.rds"))
  
  print(paste0("Model ", id.model))
  
  rejs_all <- colMeans(results, na.rm = TRUE)
  print(rejs_all)
  
  saveRDS(rejs_all,
          file = paste0(path.output,
                        "/metric/model_", id.model, "_APT.rds"))
  
  #### stop cluster ####
  stopCluster(my.cluster)
  closeAllConnections()
  
  library(beepr)
  beep(2)
}

beep(2)