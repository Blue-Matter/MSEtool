# R-hub Checks 
# Check on fedora dist
#install.packages('rhub')
library(rhub)

validate_email()

# check for CRAN 
rhub:::default_cran_check_platforms('.')
chk <- check_for_cran('../MSEtool', email='ar.hordyk@gmail.com')


# needs docker and bash installed for Windows
chk <- local_check_linux(image = "rhub/fedora-clang-devel")





v <- as.character(packageVersion("MSEtool"))
nm <- paste0("MSEtool_", v, ".tar.gz")

shell(paste0("R CMD check ../", nm, ' --as-cran'))
