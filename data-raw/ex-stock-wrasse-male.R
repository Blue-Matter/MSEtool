library(MSEtool)

## ---- wrasse-male-create-stock ----
WrasseMaleExStock <- Stock(Name       = "Example Wrasse Male Stock",
                           CommonName = "Wrasse",
                           Species    = "Thalassoma spp.")

## ---- wrasse-male-create-ages ----
Ages(WrasseMaleExStock) <- Ages(MaxAge = 15)

## ---- wrasse-male-create-length ----
Length(WrasseMaleExStock) <- Length(
  Pars     = list(Linf = c(30, 35),
                  K    = c(0.16, 0.22),
                  t0   = c(-0.6, -0.3)),
  Model    = "vonBert",
  CVatAge  = c(0.08, 0.12),
  Units    = "cm"
)

## ---- wrasse-male-create-weight ----
Weight(WrasseMaleExStock) <- Weight(
  Pars = list(alpha = 1.8E-05,
              beta  = 3.0),
  Units = 'kg'
)

## ---- wrasse-male-create-natural-mortality ----
NaturalMortality(WrasseMaleExStock) <- NaturalMortality(
  Pars = list(M = c(0.2, 0.3))
)

## ---- wrasse-male-create-maturity ----
Maturity(WrasseMaleExStock) <- Maturity(
  Pars = list(L50    = c(20, 24),
              L50_95 = c(2, 3))
)

## ---- wrasse-male-create-srr ----
SRR(WrasseMaleExStock) <- SRR(
  Pars   = list(h = c(0.7, 0.85)),
  R0     = 100,
  SD     = c(0.3, 0.5),
  AC     = c(0.1, 0.4),
  SPFrom = "Example Wrasse Female Stock",
  Units  = 1
)

## ---- wrasse-male-create-spatial ----
Spatial(WrasseMaleExStock) <- Spatial(
  UnfishedDist  = c(0.45, 0.55),
  ProbStaying   = c(0.7, 0.85),
  RelativeSize  = c(0.45, 0.55)
)

## ---- wrasse-male-create-depletion ----
Depletion(WrasseMaleExStock) <- Depletion(
  Final     = c(0.2, 0.5),
  Reference = "B0"
)

## ---- save object ----
usethis::use_data(WrasseMaleExStock, overwrite = TRUE)
