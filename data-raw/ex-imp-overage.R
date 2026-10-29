library(MSEtool)


## ---- overage-create-imp ----
OverageImp <- Imp(
  Name   = "OverageImp",
  TAC    = ImpSlot(Mean = c(1.05, 1.25), SD = c(0.05, 0.15)),
  Effort = ImpSlot(Mean = c(1.05, 1.25), SD = c(0.05, 0.15))
)


## ---- save object ----
usethis::use_data(OverageImp, overwrite = TRUE)
