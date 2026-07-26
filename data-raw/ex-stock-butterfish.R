library(MSEtool)

## ---- butterfish-create-stock ----
ButterfishExStock <- Stock(Name       = "Example Butterfish Stock",
                           CommonName = "Butterfish",
                           Species    = "Peprilus triacanthus")

## ---- butterfish-create-ages ----
Ages(ButterfishExStock) <- Ages(MaxAge = 8)

## ---- butterfish-create-length ----
Length(ButterfishExStock) <- Length(
  Pars     = list(Linf = c(38, 42),
                  K    = c(0.16, 0.24),
                  t0   = c(-0.032, -0.028)),
  Model    = "vonBert",
  CVatAge  = c(0.1, 0.15),
  Units    = "cm"
)

## ---- butterfish-create-weight ----
Weight(ButterfishExStock) <- Weight(
  Pars = list(alpha = 1.59e-05,
              beta  = 3.1)
)

## ---- butterfish-create-natural-mortality ----
NaturalMortality(ButterfishExStock) <- NaturalMortality(
  Pars = list(M = c(0.7, 0.9))
)

## ---- butterfish-create-maturity ----
Maturity(ButterfishExStock) <- Maturity(
  Pars = list(L50    = c(4.5, 10.2),
              L50_95 = c(1, 8))
)

## ---- butterfish-create-srr ----
SRR(ButterfishExStock) <- SRR(
  Pars  = list(h = c(0.4, 0.8)),
  R0    = 1000,
  SD    = c(0.7, 1.1),
  AC    = c(0.1, 0.9),
  Units = 1000
)

## ---- butterfish-create-spatial ----
Spatial(ButterfishExStock) <- Spatial(
  UnfishedDist  = c(0.095, 0.105),
  ProbStaying   = c(0.4, 0.6),
  RelativeSize  = c(0.095, 0.105)
)

## ---- butterfish-create-depletion ----
Depletion(ButterfishExStock) <- Depletion(
  Final     = c(0.05, 0.6),
  Reference = "B0"
)

## ---- save object ----
usethis::use_data(ButterfishExStock, overwrite = TRUE)


