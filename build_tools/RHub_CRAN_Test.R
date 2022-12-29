# R-hub Checks 
# Check on fedora dist
 remotes::install_github('r-hub/rhub')
library(rhub)

validate_email()

cran_prep <- check_for_cran()
cran_prep$cran_summary()

 # needs docker and bash installed for Windows
 chk <- local_check_linux(image = "rhub/fedora-clang-devel")
 
 

# 
# # check for CRAN 
# chk <- check_for_cran('../MSEtool', email='ar.hordyk@gmail.com')
# 
# 
# 
# 
# 
# v <- as.character(packageVersion("MSEtool"))
# nm <- paste0("MSEtool_", v, ".tar.gz")
# 
# shell(paste0("R CMD check ../", nm, ' --as-cran'))
