library(MSEtool)


## ---- full-compliance-create-imp ----
FullComplianceImp <- Imp(
  Name   = "FullComplianceImp",
  TAC    = ImpSlot(Mean = 1, SD = 0),
  Effort = ImpSlot(Mean = 1, SD = 0),
  Size   = ImpSlot(Compliance = 1)
)


## ---- save object ----
usethis::use_data(FullComplianceImp, overwrite = TRUE)
