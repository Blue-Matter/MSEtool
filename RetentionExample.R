library(MSEtool)
la()

nSim <- 2
Years <- 1950:2025
nArea <- 2

Ages <- Ages(10)
Length <- Length(Pars=list(Linf=100, K=0.2, t0=0),
                 Classes=seq(2.5, 120, by=5)) |>
  Populate(Ages=Ages,
           Years=Years)

set.seed(101)

Retention <- Retention() # blank Retention object

Retention |> Populate(Ages=Ages, Years=Years)
