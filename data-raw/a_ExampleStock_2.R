
library(MSEtool)

# ---- Example Stock ----

## ---- create-stock ----
ExampleStock2 <- Stock("Example Stock 2",
                      CommonName = "Butterfish",
                      Species = "Peprilus triacanthus"
)

## ---- create-ages ----
Ages(ExampleStock2) <- Ages(MaxAge = 8)

## ---- create-length ----
Length(ExampleStock2) <- Length(
  Pars = list(
    Linf = c(38, 42),
    K = c(0.16, 0.24),
    t0 = c(-0.032, -0.028)
  ),
  Model = "vonBert",
  CVatAge = c(0.1, 0.15)
)

## ---- create-weight ----
Weight(ExampleStock2) <- Weight(
  Pars = list(
    alpha = 1.59e-05,
    beta = 3.1
  )
)

## ---- create-natural-mortality ----
NaturalMortality(ExampleStock2) <- NaturalMortality(
  Pars = list(
    M = c(0.7, 0.9)
  )
)

## ---- create-maturity ----
Maturity(ExampleStock2) <- Maturity(
  Pars = list(
    L50 = c(4.5, 10.2),
    L50_95 = c(1, 8)
  )
)

## ---- create-srr ----
SRR(ExampleStock2) <- SRR(
  Pars = list(h = c(0.4, 0.8)),
  R0 = 1000,
  SD = c(0.7, 1.1),
  AC = c(0.1, 0.9)
)

## ---- create-spatial ----
Spatial(ExampleStock2) <- Spatial(
  UnfishedDist = c(0.095, 0.105),
  ProbStaying = c(0.4, 0.6),
  RelativeSize = c(0.095, 0.105)
)

## ---- create-depletion ----
Depletion(ExampleStock2) <- Depletion(
  Final = c(0.05, 0.6),
  Reference = "B0"
)

## save object
usethis::use_data(ExampleStock2, overwrite = TRUE)


