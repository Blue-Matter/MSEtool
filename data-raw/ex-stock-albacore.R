
library(MSEtool)

## ---- create-stock ----
AlbacoreExStock <- Stock("Example Albacore Stock",
                         CommonName = "Albacore",
                         Species = "Thunnus alalunga"
)

## ---- create-ages ----
Ages(AlbacoreExStock) <- Ages(MaxAge = 20)

## ---- create-length ----
Length(AlbacoreExStock) <- Length(
  Pars = list(
    Linf = c(121, 135),
    K = c(0.16, 0.22),
    t0 = c(-1.86, -1.41)
  ),
  Model = "vonBert",
  CVatAge = c(0.1, 0.15)
)

## ---- create-weight ----
Weight(AlbacoreExStock) <- Weight(
  Pars = list(
    alpha = 1.34E-05,
    beta = 3.106
  )
)

## ---- create-natural-mortality ----
NaturalMortality(AlbacoreExStock) <- NaturalMortality(
  Pars = list(
    M = c(0.35, 0.45)
  )
)

## ---- create-maturity ----
Maturity(AlbacoreExStock) <- Maturity(
  Pars = list(
    L50 = c(81, 91),
    L50_95 = c(10, 12)
  )
)

## ---- create-srr ----
SRR(AlbacoreExStock) <- SRR(
  Pars = list(h = c(0.65, 0.85)),
  R0 = 1000,
  SD = c(0.15, 0.3),
  AC = c(0.1, 0.9)
)

## ---- create-spatial ----
Spatial(AlbacoreExStock) <- Spatial(
  UnfishedDist = c(0.095, 0.105),
  ProbStaying = c(0.8, 0.9),
  RelativeSize = c(0.095, 0.105)
)

## ---- create-depletion ----
Depletion(AlbacoreExStock) <- Depletion(
  Final = c(0.05, 0.6),
  Reference = "B0"
)

## save object
usethis::use_data(AlbacoreExStock, overwrite = TRUE)


