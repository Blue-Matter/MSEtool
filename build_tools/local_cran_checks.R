devtools::build('../MSEtool')

v <- as.character(packageVersion("MSEtool"))
nm <- paste0("MSEtool_", v, ".tar.gz")

shell(paste0("R CMD check ../", nm))

# Check downstream dependencies
# currently none

usethis::use_revdep()
# revdepcheck::revdep_check(num_workers = 4)

tt = devtools::spell_check()

tt

devtools::release()

