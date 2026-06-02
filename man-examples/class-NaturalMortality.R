# See NaturalMortalityModels() for all available built-in natural mortality
# models and the parameter names required for each.
NaturalMortalityModels()

# ---- Model-based specification ----

## Constant M (scalar) — same value for every simulation and year
nm <- NaturalMortality(Pars = list(M = 0.2))

## Stochastic across simulations — M drawn from Uniform(lower, upper)
## once per simulation
nm <- NaturalMortality(Pars = list(M = c(0.1, 0.3)))

## Inter-annual random walk on M — log-normal with 10% CV
## MSD is removed from Pars after use
nm <- NaturalMortality(Pars = list(M   = c(0.1, 0.3),
                                   Msd = 0.1))

## Time-varying M with named change-point arrays
## Only the years where values change need to be supplied; Extend() fills
## all intermediate and future years by forward-filling automatically.
nSim <- 48

## M increases in 2010 (two change points: 1990 and 2010)
M_arr <- array(
  c(rep(0.2, nSim), rep(0.3, nSim)),
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
nm <- NaturalMortality(Pars = list(M = M_arr))

## Combine stochastic (across sims) and time-varying (across years)
M_sim  <- runif(nSim, 0.1, 0.3)
M_arr2 <- array(
  c(M_sim, M_sim * 1.2),              # 20% increase from 2010 onward
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
nm <- NaturalMortality(Pars = list(M = M_arr2))

# ---- Direct array specification ----

## Supply MeanAtAge directly (Pars left empty).
## A plain numeric vector of length nAge is accepted; treated as a single
## simulation, single year and replicated by Extend() as needed.
ages  <- 0:20
nm_aa <- NaturalMortality(
  MeanAtAge = seq(0.5, 0.1, length.out = length(ages))
)

## Named Sim x Age x Year array — age-varying M that shifts in 2010.
## Extend() replicates Sim = 1 to all nSim simulations automatically.
maa <- array(
  c(seq(0.5, 0.1, length.out = length(ages)),   # 1990 schedule
    seq(0.6, 0.15, length.out = length(ages))),  # 2010 schedule (M higher)
  dim      = c(1, length(ages), 2),
  dimnames = list(Sim = 1, Age = ages, Year = c(1990, 2010))
)
nm_arr <- NaturalMortality(MeanAtAge = maa)

## Supply MeanAtLength directly — converted to MeanAtAge via the ALK
## during Populate() when a Length object is provided.
lens  <- seq(5, 120, by = 5)
nm_al <- NaturalMortality(
  MeanAtLength = array(
    0.4 * exp(-0.02 * lens),          # M declining with length
    dim      = c(1, length(lens), 1),
    dimnames = list(Sim = 1, Length = lens, Year = 1990)
  )
)

# ---- Slot accessors ----

nm <- NaturalMortality(Pars = list(M = 0.2))

## Read slots
MeanAtAge(nm)     # NULL until Populate() is called
Units(nm)

## Replace slots
Units(nm)  <- "quarter"
Classes(nm) <- 0:20

# ---- Attaching to a Stock ----

stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
NaturalMortality(stk) <- NaturalMortality(Pars = list(M = 0.2))
NaturalMortality(stk)

# ---- Populate ----

## The final populated object (typically done internally).
## At-length models require a populated Length object passed to Populate().
pop_nm <- Populate(NaturalMortality(stk), Ages = Ages(MaxAge = 20))
pop_nm

MeanAtAge(pop_nm)
