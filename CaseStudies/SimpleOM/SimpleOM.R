library(MSEtool)


## ---- initialize-om ----
SimpleOM <- OM('Example OM',
               nYear=30,
               pYear=20,
               nSim=48)

## ---- access-assign-1 ----
nSim(SimpleOM)
nSim(SimpleOM) <- 5
nSim(SimpleOM)

## ---- access-assign-2 ----
nYear(SimpleOM) <- 20
pYear(SimpleOM) <- 10

## ---- current-year-assign ----
CurrentYear(SimpleOM) <- 2020

## ---- change-seasons ----
Seasons(SimpleOM) <- 4 # 4-quarters
Years(SimpleOM, 'H') |> head(8)

## ---- date-convert ----
SimpleOM |> 
  Years() |> 
  lubridate::date_decimal() |>
  lubridate::as_date() |>
  head()

## ---- initialize-stock ----
SimpleStock <- Stock('Example Stock',
                     CommonName = 'Albacore',
                     Species = "Thunnus alalunga")

## ---- create-ages_1 ----
Ages(SimpleStock) <- Ages(MaxAge=4*20, Units='quarter')
Ages(SimpleStock)

## ---- create-ages_2 ----
Seasons(SimpleOM) <- 1 # annual
Ages(SimpleStock) <- Ages(MaxAge=20)

## ---- create-length ----
Length(SimpleStock) <- Length(Pars=list())




