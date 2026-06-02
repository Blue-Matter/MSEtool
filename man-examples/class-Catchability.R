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

cq_scalar <- Catchability(Efficiency = 0.01)

cq_scalar_pop <- Populate(
  cq_scalar,
  nSim      = nSim,
  HistYears = hist_years,
  ProjYears = proj_years
)
Efficiency(cq_scalar_pop)

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

cq_array_pop <- Populate(
  cq_array,
  nSim      = nSim,
  HistYears = hist_years,
  ProjYears = proj_years
)
Efficiency(cq_array_pop)

## ---- Projected-year trend with qInc ----
# qInc applies a compounded annual percentage change to efficiency in
# projected years only. Historical efficiency is unaffected.
# Here a 2% annual increase is applied: efficiency in projection year t
# is scaled by (1 + 2/100)^t relative to the terminal historical value.
# Negative values model declining efficiency (e.g. gear deterioration).
cq_inc <- Catchability(Efficiency = 0.01, qInc = 2)

cq_inc_pop <- Populate(
  cq_inc,
  nSim      = nSim,
  HistYears = hist_years,
  ProjYears = proj_years
)
Efficiency(cq_inc_pop)  # efficiency rises in proj_years; hist_years unchanged

## ---- Projected-year stochasticity with qCV ----
# qCV introduces lognormal inter-annual variation in efficiency during
# projected years only. Each projected year's efficiency is multiplied by
# a mean-1 lognormal deviate with the specified coefficient of variation.
# Historical efficiency is unaffected.
# Note: for most applications, supplying a fully specified Efficiency array
# gives more direct control than using qInc or qCV.
cq_cv <- Catchability(Efficiency = 0.01, qCV = 0.1)

cq_cv_pop <- Populate(
  cq_cv,
  nSim      = nSim,
  HistYears = hist_years,
  ProjYears = proj_years,
  seed      = 42
)
Efficiency(cq_cv_pop)  # stochastic variation in proj_years; hist_years constant

## ---- Combined trend and stochasticity ----
cq_both <- Catchability(Efficiency = 0.01, qInc = 2, qCV = 0.1)

cq_both_pop <- Populate(
  cq_both,
  nSim      = nSim,
  HistYears = hist_years,
  ProjYears = proj_years,
  seed      = 42
)
Efficiency(cq_both_pop)

## ---- Accessor and replacement functions ----
cq <- Catchability(Efficiency = 0.01, qCV = 0.1, qInc = 1)

Efficiency(cq)
qCV(cq)
qInc(cq)

Efficiency(cq) <- 0.02
Efficiency(cq)

qInc(cq) <- -1   # declining efficiency in projection
qInc(cq)

## ---- Attaching to a Fleet object ----
f <- Fleet(Name = "Trawl")
Catchability(f) <- Catchability(Efficiency = 0.01)
Catchability(f)
f |> Catchability() |> Efficiency()

