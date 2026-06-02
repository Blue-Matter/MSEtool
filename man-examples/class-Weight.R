# See WeightModels() for all available built-in weight models and the
# parameter names required for each.
WeightModels()

# ---- Model-based specification ----

## Constant parameters (scalar) — same value for every simulation and year
wt <- Weight(Pars = list(a = 0.01, b = 3.0))

## Stochastic across simulations — a drawn from Uniform(lower, upper)
## once per simulation; b fixed in all simulations
wt <- Weight(Pars = list(a = c(0.008, 0.012), b = 3.0))

## Inter-annual random walk on a — log-normal with 10% CV
## aSD is removed from Pars after use
wt <- Weight(Pars = list(a    = c(0.008, 0.012),
                         aSD  = 0.1,
                         b    = 3.0))

## Time-varying parameters with named change-point arrays
## Only the years where values change need to be supplied; Extend() fills
## all intermediate and future years by forward-filling automatically.
nSim <- 48

## a increases in 2010 (two change points: 1990 and 2010)
a_arr <- array(
  c(rep(0.01, nSim), rep(0.012, nSim)),
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
wt <- Weight(Pars = list(a = a_arr, b = 3.0))

## Combine stochastic (across sims) and time-varying (across years):
## a is drawn per simulation AND increases in 2010
a_sim  <- runif(nSim, 0.008, 0.012)
a_arr2 <- array(
  c(a_sim, a_sim * 1.2),              # 20% increase from 2010 onward
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
wt <- Weight(Pars = list(a = a_arr2, b = 3.0))

# ---- Direct array specification ----

## Supply MeanAtAge directly (Pars left empty).
## A plain numeric vector of length nAge is accepted; it is treated as a
## single simulation, single year and replicated by Extend() as needed.
ages  <- 0:20
Linf  <- 100; K <- 0.2; t0 <- -0.1; a <- 0.01; b <- 3
wt_aa <- Weight(MeanAtAge = a * (Linf * (1 - exp(-K * (ages - t0))))^b)

## Named Sim x Age x Year array — two change-point years, single simulation.
## Extend() replicates Sim = 1 to all nSim simulations automatically.
maa <- array(
  c(a * (Linf       * (1 - exp(-K * (ages - t0))))^b,   # 1990 schedule
    a * (Linf * 1.1 * (1 - exp(-K * (ages - t0))))^b),  # 2010 (Linf up 10%)
  dim      = c(1, length(ages), 2),
  dimnames = list(Sim = 1, Age = ages, Year = c(1990, 2010))
)
wt_arr <- Weight(MeanAtAge = maa)

## Supply MeanAtLength directly — converted to MeanAtAge via the ALK
## during Populate() when a Length object is provided.
lens      <- seq(5, 120, by = 5)
wt_al <- Weight(
  MeanAtLength = array(
    a * lens^b,
    dim      = c(1, length(lens), 1),
    dimnames = list(Sim = 1, Length = lens, Year = 1990)
  )
)

# ---- Slot accessors ----

wt <- Weight(Pars = list(a = 0.01, b = 3.0))

## Read slots
MeanAtAge(wt)     # NULL until Populate() is called
MeanAtLength(wt)  # NULL until Populate() is called (at-length models only)
CVatAge(wt)       # NULL by default; AWK is only built when non-NULL
Units(wt)

## Replace slots
Units(wt)   <- "kg"
TruncSD(wt) <- 3

# ---- Attaching to a Stock ----

stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
Weight(stk) <- Weight(Pars = list(a = 0.01, b = 3.0))
Weight(stk)

# ---- Populate ----

## The final populated object (typically done internally).
## An at-length model requires a populated Length object.
pop_wt <- Populate(Weight(stk), Ages = Ages(MaxAge = 20))
pop_wt
