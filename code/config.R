#################   Configuration   #################
## Purpose:                                           
#     Store fixed program configuration variables.
#####################################################

#### set path ####
path.code <- paste0(getwd(), "/code/")
path.utils <- paste0(path.code, "utils/")
path.methods <- paste0(path.code, "methods/")
path.data <- paste0(getwd(), "/data/")
path.output <- paste0(getwd(), "/output/")
path.output.raw <- paste0(path.output, "raw/")
path.output.merge <- paste0(path.output, "merge/")
path.output.metric <- paste0(path.output, "metric/")

#### load libraries ####
file.sources = list.files(c(path.utils), pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)

packages.cran <- c("abind", "beepr", "Hotelling", "parallel", "penalizedSVM",
                   "msda", "catch", "tensr", "rTensor")
packages.github.mine <- c()
packages.github.cran <- c()
load_silently(packages.cran, packages.github.mine, packages.github.cran)
RNGkind("L'Ecuyer-CMRG")

#### source functions ####
file.sources = list.files(c(path.methods), pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)
source(paste0(path.code, "get_paras.R"))

#### create folders ####
check_create_folder(c(path.output, path.output.raw, path.output.merge, 
                      path.output.metric))

