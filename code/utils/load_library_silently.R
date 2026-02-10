install_package_list <- function(packages, source = "cran") {
  if (!is.null(packages)) {
    new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
    if (length(new.packages)) {
      switch (source,
              "cran" = install.packages(new.packages),
              "git.mine" = {
                for (package in new.packages) {
                  tryCatch(install_github(paste0("NeveTong/", package)),
                           error = function(cond) message(cond))
                }
              },
              "git.cran" = {
                for (package in new.packages) {
                  tryCatch(install_github(paste0("cran/", package)),
                           error = function(cond) message(cond))
                }
              }
      )
    }
  }
}

load_silently = function(packages.cran = NULL, packages.github.mine = NULL, packages.github.cran = NULL){
  install_package_list(packages.cran, source = "cran")
  install_package_list(packages.github.mine, source = "git.mine")
  install_package_list(packages.github.cran, source = "git.cran")

  for(package in c(packages.cran, packages.github.mine, packages.github.cran)){
    suppressPackageStartupMessages(library(package, character.only = TRUE))    
  }
  invisible()
}