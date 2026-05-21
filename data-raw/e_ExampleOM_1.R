library(MSEtool)

# ---- Example OM  ----

## ---- create-om ----
ExampleOM <- OM("Single Stock - Single Fleet",
  nYear = 30,
  pYear = 20,
  nSim = 5
)

Stock(ExampleOM) <- ExampleStock
Fleet(ExampleOM) <- ExampleFleet

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
