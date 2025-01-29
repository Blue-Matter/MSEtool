## code to prepare `ControlDefault` dataset goes here

ControlDefault <- list()
boundsF <- c(1E-3, 1)
FSearch <- exp(seq(log(min(boundsF)), log(max(boundsF)), length.out = 50))
ControlDefault$Curves <- list(FSearch=FSearch)

usethis::use_data(ControlDefault, overwrite = TRUE)
