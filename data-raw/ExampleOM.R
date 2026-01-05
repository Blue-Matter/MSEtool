library(MSEtool)

# ---- Example OM  ----

## ---- create-om ----
ExampleOM <- OM("Example OM",
  nYear = 30,
  pYear = 20,
  nSim = 5
)

# ---- Example Stock ----

## ---- create-stock ----
ExampleStock <- Stock("Example Stock",
  CommonName = "Albacore",
  Species = "Thunnus alalunga"
)

## ---- create-ages ----
Ages(ExampleStock) <- Ages(MaxAge = 20)

## ---- create-length ----
Length(ExampleStock) <- Length(
  Pars = list(
    Linf = c(121, 135),
    K = c(0.16, 0.22),
    t0 = c(-1.86, -1.41)
  ),
  Model = "vonBert",
  CVatAge = c(0.1, 0.15)
)

## ---- create-weight ----
Weight(ExampleStock) <- Weight(
  Pars = list(
    alpha = 1.34E-05,
    beta = 3.106
  )
)

## ---- create-natural-mortality ----
NaturalMortality(ExampleStock) <- NaturalMortality(
  Pars = list(
    M = c(0.35, 0.45)
  )
)

## ---- create-maturity ----
Maturity(ExampleStock) <- Maturity(
  Pars = list(
    L50 = c(81, 91),
    L50_95 = c(10, 12)
  )
)

## ---- create-srr ----
SRR(ExampleStock) <- SRR(
  Pars = list(h = c(0.65, 0.85)),
  R0 = 1000,
  SD = c(0.15, 0.3),
  AC = c(0.1, 0.9)
)

## ---- create-spatial ----
Spatial(ExampleStock) <- Spatial(
  UnfishedDist = c(0.095, 0.105),
  ProbStaying = c(0.8, 0.9),
  RelativeSize = c(0.095, 0.105)
)

## ---- create-depletion ----
Depletion(ExampleStock) <- Depletion(
  Final = c(0.05, 0.6),
  Reference = "B0"
)


# ---- Example Fleet ----

## ---- create-fleet ----
ExampleFleet <- Fleet("Example Fleet")


## ---- stochastic effort ----
Effort(ExampleFleet) <- Effort(Value = data.frame(
  Year = c(0, 0.3, 0.6, 1.0),
  Lower = c(0, 0.4, 0.4, 1),
  Upper = c(0, 0.6, 0.6, 1),
  CV = 0.1
))


## ---- selectivity ----
Selectivity(ExampleFleet) <- Selectivity(Pars = list(
  L5 = c(0.2, 0.4),
  LFS = c(0.75, 1.1),
  Vmaxlen = c(0.5, 1)
), isRel = TRUE)


# ---- Example Obs ----


# ---- Example OM  Cont. ----

Stock(ExampleOM) <- ExampleStock
Fleet(ExampleOM) <- ExampleFleet


## ---- Save Data Objects -----

usethis::use_data(ExampleStock, overwrite = TRUE)
usethis::use_data(ExampleFleet, overwrite = TRUE)
usethis::use_data(ExampleOM, overwrite = TRUE)

# 
# ## ---- access-assign-1 ----
# nSim(ExampleOM)
# nSim(ExampleOM) <- 5
# nSim(ExampleOM)
# 
# ## ---- access-assign-2 ----
# nYear(ExampleOM) <- 20
# pYear(ExampleOM) <- 10
# 
# ## ---- current-year-read ----
# CurrentYear(ExampleOM)
# 
# ## ---- current-year-assign ----
# CurrentYear(ExampleOM) <- 2020
# 
# ## ---- years-read ----
# Years(ExampleOM, "H")
# Years(ExampleOM, "P")
# 
# ## ---- change-seasons ----
# Seasons(ExampleOM) <- 4 # 4-quarters
# Years(ExampleOM, "H") |> head(8)
# 
# ## ---- om-slotnamees ----
# slotNames(ExampleOM)
# ExampleOM@nSim
# 
# ## ---- print-seasons ----
# `Seasons<-`
# 
# ## ---- date-convert ----
# ExampleOM |>
#   Years() |>
#   lubridate::date_decimal() |>
#   lubridate::as_date() |>
#   head()
# 

#
#
#
#
#
#
#
#
# ## ---- create-ages_1 ----
# Ages(ExampleStock) <- Ages(MaxAge = 4 * 20, Units = "quarter")
#
# ## ---- classes-ages ----
# Classes(ExampleStock)
#
# ## ---- create-ages_2 ----
# Seasons(ExampleOM) <- 1 # annual
# Ages(ExampleStock) <- Ages(MaxAge = 20)
# Classes(ExampleStock)
#
#
# ## ---- populate-length ----
# ExampleLengthComplete <- Length(ExampleStock) |>
#   Populate(
#     Ages = Ages(ExampleStock),
#     nSim = nSim(ExampleOM),
#     Years = Years(ExampleOM)
#   )
#
# Pars(ExampleLengthComplete)
# MeanAtAge(ExampleLengthComplete)
# CVatAge(ExampleLengthComplete)
#
#
# ## ---- populate-M ----
# ExampleStock |>
#   NaturalMortality() |>
#   Populate(
#     Years = 2023:2025
#   ) |>
#   Pars()
#
#
# ## ---- populate-maturity ----
# ExampleMaturityComplete <- ExampleStock |>
#   Maturity() |>
#   Populate(
#     Ages = Ages(ExampleStock),
#     Length = Length(ExampleStock),
#     Years = Years(ExampleOM)
#   )
#
# ExampleMaturityComplete |> Pars()
# ExampleMaturityComplete |> MeanAtLength()
# ExampleMaturityComplete |> MeanAtAge()
#
#
# # ---- Example Fleet ----
#
#
#
#
#
#
#
#
#
# # ---- Example Obs ----
#
#
# # ---- Example OM  Cont. ----
#
#
# ## ---- access-assign-2 ----
# nYear(ExampleOM) <- 20
# pYear(ExampleOM) <- 10
#
# ## ---- current-year-read ----
# CurrentYear(ExampleOM)
#
# ## ---- current-year-assign ----
# CurrentYear(ExampleOM) <- 2020
#
# ## ---- years-read ----
# Years(ExampleOM, "H")
# Years(ExampleOM, "P")
#
# ## ---- change-seasons ----
# Seasons(ExampleOM) <- 4 # 4-quarters
# Years(ExampleOM, "H") |> head(8)
#
# ## ---- om-slotnamees ----
# slotNames(ExampleOM)
# ExampleOM@nSim
#
# ## ---- print-seasons ----
# `Seasons<-`
#
# ## ---- date-convert ----
# ExampleOM |>
#   Years() |>
#   lubridate::date_decimal() |>
#   lubridate::as_date() |>
#   head()
#
#
#
#
