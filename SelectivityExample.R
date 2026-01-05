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

Selectivity <- Selectivity() # blank Selectivity object

SelectivityModels(full=FALSE, print=FALSE) # 


# UP TO HERE - update below to 

# - add dimension names where needed
# - update Retention & Discard Mortality 

# - continue development in SelectivityExample.R
# - update Simulate and SimulateDyanmics to deal with area-based select, retain, and discardMort
# - keep OM@Fleet as a list rather than combine over Fleets??

# Logistic - constant over Sim, Time, & Area
Selectivity <- Selectivity(Pars=list(A50=3, 
                                     A50_95=1)) |>
  Populate(Ages=Ages, 
           Length=Length,
           Years=Years,
           nSim=nSim)

Selectivity |> MeanAtLength()
Selectivity |> MeanAtAge()



# Logistic - constant over Sim, Time, & Area
Selectivity <- Selectivity(Pars=list(SL50=30, 
                                     SL50_95=10)) |>
  Populate(Ages=Ages, 
           Length=Length,
           Years=Years,
           nSim=nSim)

Selectivity |> MeanAtLength()
Selectivity |> MeanAtAge()

# Logistic - constant over Sim & Area, random walk over time 
Selectivity <- Selectivity(Pars=list(SL50=30, 
                                     SL50sd=0.1,
                                     SL50_95=10)) |>
  Populate(Ages=Ages, 
           Length=Length,
           Years=Years,
           nSim=nSim)

Selectivity |> MeanAtLength()
Selectivity |> MeanAtAge()


# Logistic - constant over Sim & Time, vary over Areas
Selectivity <- Selectivity(Pars=list(SL50=array(c(30, 60),
                                                dim=c(1,1,2)), 
                                     SL50_95=10)) |>
  Populate(Ages=Ages, 
           Length=Length,
           Years=Years,
           nSim=nSim,
           nArea=nArea)

Selectivity |> MeanAtLength()
Selectivity |> MeanAtAge()


# Logistic - vary over Sim
Selectivity <- Selectivity(Pars=list(SL50=c(30, 40), 
                                     SL50_95=10)) |>
  Populate(Ages=Ages, 
           Length=Length,
           Years=Years,
           nSim=nSim)

Selectivity |> MeanAtLength()
Selectivity |> MeanAtAge()


# Logistic - vary over Time
Selectivity <- Selectivity(Pars=list(SL50=c(30, 40), 
                                     SL50_95=10)) |>
  Populate(Ages=Ages, 
           Length=Length,
           Years=Years,
           nSim=nSim)

Selectivity |> MeanAtLength()
Selectivity |> MeanAtAge()



# Logistic - vary over Areas


# Custom Function 

