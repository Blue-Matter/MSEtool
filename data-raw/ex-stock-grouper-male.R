library(MSEtool)

## ---- grouper-male-create-stock ----
GrouperMaleExStock <- Stock(Name       = "Example Grouper Male Stock",
                            CommonName = "Grouper",
                            Species    = "Epinephelus spp.")

## ---- grouper-male-create-ages ----
Ages(GrouperMaleExStock) <- Ages(MaxAge = 30)

## ---- grouper-male-create-length ----
Length(GrouperMaleExStock) <- Length(
  Pars     = list(Linf = c(95, 110),
                  K    = c(0.10, 0.15),
                  t0   = c(-1, -0.5)),
  Model    = "vonBert",
  CVatAge  = c(0.08, 0.12),
  Units    = "cm"
)

## ---- grouper-male-create-weight ----
Weight(GrouperMaleExStock) <- Weight(
  Pars = list(alpha = 1.2E-05,
              beta  = 3.05),
  Units = 'kg'
)

## ---- grouper-male-create-natural-mortality ----
NaturalMortality(GrouperMaleExStock) <- NaturalMortality(
  Pars = list(M = c(0.12, 0.18))
)

## ---- grouper-male-create-maturity ----
Maturity(GrouperMaleExStock) <- Maturity(
  Pars = list(L50    = c(45, 55),
              L50_95 = c(5, 8))
)

## ---- grouper-male-create-srr ----
# all fish are born female: males come only from the Herm transition
SRR(GrouperMaleExStock) <- SRR(
  R0     = 0,
  SPFrom = "Example Grouper Female Stock",
  Units  = 1
)

## ---- grouper-male-create-spatial ----
Spatial(GrouperMaleExStock) <- Spatial(
  UnfishedDist  = c(0.45, 0.55),
  ProbStaying   = c(0.7, 0.85),
  RelativeSize  = c(0.45, 0.55)
)

## ---- save object ----
usethis::use_data(GrouperMaleExStock, overwrite = TRUE)
