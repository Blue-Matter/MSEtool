library(MSEtool)


## ---- underage-create-imp ----
UnderageImp <- Imp(
  Name   = "UnderageImp",
  TAC    = ImpSlot(Mean = c(0.7, 0.9), SD = c(0.05, 0.15)),
  Effort = ImpSlot(Mean = c(0.7, 0.9), SD = c(0.05, 0.15))
)


## ---- save object ----
usethis::use_data(UnderageImp, overwrite = TRUE)
