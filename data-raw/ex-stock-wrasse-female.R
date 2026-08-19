library(MSEtool)

## ---- wrasse-female-create-stock ----
WrasseFemaleExStock <- Stock(Name       = "Example Wrasse Female Stock",
                             CommonName = "Wrasse",
                             Species    = "Thalassoma spp.")

## ---- wrasse-female-create-ages ----
Ages(WrasseFemaleExStock) <- Ages(MaxAge = 15)

## ---- wrasse-female-create-length ----
Length(WrasseFemaleExStock) <- Length(
  Pars     = list(Linf = c(23, 27),
                  K    = c(0.22, 0.28),
                  t0   = c(-0.6, -0.3)),
  Model    = "vonBert",
  CVatAge  = c(0.08, 0.12),
  Units    = "cm"
)

## ---- wrasse-female-create-weight ----
Weight(WrasseFemaleExStock) <- Weight(
  Pars = list(alpha = 1.8E-05,
              beta  = 3.0),
  Units = 'kg'
)

## ---- wrasse-female-create-natural-mortality ----
NaturalMortality(WrasseFemaleExStock) <- NaturalMortality(
  Pars = list(M = c(0.3, 0.4))
)

## ---- wrasse-female-create-maturity ----
Maturity(WrasseFemaleExStock) <- Maturity(
  Pars = list(L50    = c(11, 14),
              L50_95 = c(1.5, 2.5))
)

## ---- wrasse-female-create-srr ----
SRR(WrasseFemaleExStock) <- SRR(
  Pars  = list(h = c(0.7, 0.85)),
  R0    = 5000,
  SD    = c(0.3, 0.5),
  AC    = c(0.1, 0.4),
  Units = 1
)

## ---- wrasse-female-create-spatial ----
Spatial(WrasseFemaleExStock) <- Spatial(
  UnfishedDist  = c(0.45, 0.55),
  ProbStaying   = c(0.7, 0.85),
  RelativeSize  = c(0.45, 0.55)
)

## ---- wrasse-female-create-depletion ----
Depletion(WrasseFemaleExStock) <- Depletion(
  Final     = c(0.2, 0.5),
  Reference = "B0"
)

## ---- save object ----
usethis::use_data(WrasseFemaleExStock, overwrite = TRUE)
