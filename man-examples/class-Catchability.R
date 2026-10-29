## ---- Empty Catchability object ----
# An empty Catchability object is created by default when constructing a Fleet.
# PopulateCatchability() initialises Efficiency to 1 across all simulations
# and years when the Efficiency slot is NULL.
cq <- Catchability()
cq

## ---- Constant efficiency (scalar) ----
# A scalar is the most common form. It is treated as constant across all
# simulations and years: equivalent to supplying a full nSim x nYear array
# of that value.
hist_years <- 2000:2024
proj_years <- 2025:2034
nSim       <- 48

pop <- function(cq, ...) {
  Populate(cq, nSim = nSim, HistYears = hist_years, ProjYears = proj_years, ...)
}

cq_scalar <- Catchability(Efficiency = 0.01)
Efficiency(pop(cq_scalar))

## ---- Time-varying efficiency (array) ----
# A Sim x Year array allows efficiency to vary across years and simulations.
# Here efficiency increases linearly from 0.005 to 0.02 over the historical
# period (one row, replicated across simulations internally).
eff_array <- array(
  seq(0.005, 0.02, length.out = length(hist_years)),
  dim      = c(1, length(hist_years)),
  dimnames = list(Sim = 1, Year = hist_years)
)
cq_array <- Catchability(Efficiency = eff_array)
Efficiency(pop(cq_array))

## ---- Projected-year trend with qInc ----
# qInc applies a compounded annual percentage change to efficiency in
# projected years only. Historical efficiency is unaffected.
# Here a 2% annual increase is applied: efficiency in projection year t
# is scaled by (1 + 2/100)^t relative to the terminal historical value.
# Negative values model declining efficiency (e.g. gear deterioration).
cq_inc <- Catchability(Efficiency = 0.01, qInc = 2)
Efficiency(pop(cq_inc))  # efficiency rises in proj_years; hist_years unchanged

## ---- Projected-year stochasticity with qCV ----
# qCV introduces lognormal inter-annual variation in efficiency during
# projected years only, via a mean-1 lognormal deviate with the specified
# coefficient of variation. Historical efficiency is unaffected.
# For most applications, a fully specified Efficiency array gives more
# direct control than using qInc or qCV. The two can also be combined.
cq_cv <- Catchability(Efficiency = 0.01, qCV = 0.1)
Efficiency(pop(cq_cv, seed = 42))  # stochastic in proj_years; hist_years constant

## ---- Accessor and replacement functions ----
cq <- Catchability(Efficiency = 0.01, qCV = 0.1, qInc = 1)

Efficiency(cq)
qCV(cq)
qInc(cq)

Efficiency(cq) <- 0.02
qInc(cq)       <- -1   # declining efficiency in projection

## ---- Attaching to a Fleet object ----
f <- Fleet(Name = "Trawl")
Catchability(f) <- Catchability(Efficiency = 0.01)
f |> Catchability() |> Efficiency()
