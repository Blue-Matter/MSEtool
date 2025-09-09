## code to prepare `ControlDefault` dataset goes here

ControlDefault <- list()
# FSearch <- seq(from=0, to=1, by=0.005) 
# # boundsF <- c(1E-3, 1)
# # FSearch <- exp(seq(log(min(boundsF)), log(max(boundsF)), length.out = 200))
# ControlDefault$Curves <- list(FSearch=FSearch)

ControlDefault$RefYield <- list()
ControlDefault$RefYield$lastnTS <- 5


ControlDefault$RefPointTimeSteps <- NULL

usethis::use_data(ControlDefault, overwrite = TRUE)



