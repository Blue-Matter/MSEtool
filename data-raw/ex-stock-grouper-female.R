library(MSEtool)

## ---- grouper-female-create-stock ----
GrouperFemaleExStock <- Stock(Name       = "Example Grouper Female Stock",
                              CommonName = "Grouper",
                              Species    = "Epinephelus spp.")

## ---- grouper-female-create-ages ----
Ages(GrouperFemaleExStock) <- Ages(MaxAge = 30)

## ---- grouper-female-create-length ----
Length(GrouperFemaleExStock) <- Length(
  Pars     = list(Linf = c(85, 100),
                  K    = c(0.10, 0.15),
                  t0   = c(-1, -0.5)),
  Model    = "vonBert",
  CVatAge  = c(0.08, 0.12),
  Units    = "cm"
)

## ---- grouper-female-create-weight ----
Weight(GrouperFemaleExStock) <- Weight(
  Pars = list(alpha = 1.2E-05,
              beta  = 3.05),
  Units = 'kg'
)

## ---- grouper-female-create-natural-mortality ----
NaturalMortality(GrouperFemaleExStock) <- NaturalMortality(
  Pars = list(M = c(0.12, 0.18))
)

## ---- grouper-female-create-maturity ----
Maturity(GrouperFemaleExStock) <- Maturity(
  Pars = list(L50    = c(45, 55),
              L50_95 = c(5, 8))
)

## ---- grouper-female-create-srr ----
SRR(GrouperFemaleExStock) <- SRR(
  Pars  = list(h = c(0.7, 0.85)),
  R0    = 5000,
  SD    = c(0.3, 0.6),
  AC    = c(0.1, 0.4),
  Units = 1
)

## ---- grouper-female-create-spatial ----
Spatial(GrouperFemaleExStock) <- Spatial(
  UnfishedDist  = c(0.45, 0.55),
  ProbStaying   = c(0.7, 0.85),
  RelativeSize  = c(0.45, 0.55)
)

## ---- grouper-female-create-depletion ----
Depletion(GrouperFemaleExStock) <- Depletion(
  Final     = c(0.2, 0.5),
  Reference = "B0"
)

## ---- save object ----
usethis::use_data(GrouperFemaleExStock, overwrite = TRUE)
