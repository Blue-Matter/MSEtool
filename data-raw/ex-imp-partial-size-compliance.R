library(MSEtool)


## ---- partial-size-compliance-create-imp ----
PartialSizeComplianceImp <- Imp(
  Name   = "PartialSizeComplianceImp",
  TAC    = ImpSlot(Mean = 1, SD = 0),
  Effort = ImpSlot(Mean = 1, SD = 0),
  Size   = ImpSlot(Compliance = 0.6)
)


## ---- save object ----
usethis::use_data(PartialSizeComplianceImp, overwrite = TRUE)
