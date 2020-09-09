Stock <- new("Stock", "build_tools/Objects/Stock/Albacore.csv")
Fleet <- new("Fleet", "build_tools/Objects/Fleet/Generic_Fleet.csv")
Obs  <- new("Obs", "build_tools/Objects/Obs/Generic_Obs.csv")
Imp <- new("Imp", "build_tools/Objects/Imp/Perfect_Imp.csv")

testOM <- new('OM', Stock, Fleet, Obs, Imp, nsim=3)
usethis::use_data(testOM)

#' An example OM object for testing purposes
#'
"testOM"