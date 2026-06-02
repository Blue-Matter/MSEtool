# See LengthModels() for all available built-in growth models and the
# parameter names required for each.
LengthModels()

# ---- Model-based specification ----

## Constant parameters (scalar) — same value for every simulation and year
len <- Length(Pars = list(Linf = 100, K = 0.2, t0 = -0.1))

## Stochastic across simulations — Linf and K drawn from Uniform(lower, upper)
## once per simulation; t0 fixed at -0.1 in all simulations
len <- Length(Pars = list(Linf = c(90, 110),
                          K    = c(0.15, 0.25),
                          t0   = -0.1))

## Inter-annual random walk on Linf — log-normal with 5% CV
## LinfSD is removed from Pars after use
len <- Length(Pars = list(Linf    = c(90, 110),
                          LinfSD  = 0.05,
                          K       = c(0.15, 0.25),
                          t0      = -0.1))

## Time-varying parameters with named change-point arrays
## Only the years where values change need to be supplied; Extend() fills
## all intermediate and future years by forward-filling automatically.
nSim <- 48
## Linf increases in 2010 (two change points: 1990 and 2010)
Linf_arr <- array(
  c(rep(80, nSim), rep(90, nSim)),
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
len <- Length(Pars = list(Linf = Linf_arr,
                          K    = c(0.15, 0.25),
                          t0   = -0.1))

## Combine stochastic (across sims) and time-varying (across years):
## Linf is drawn per simulation AND changes in 2010
Linf_sim   <- runif(nSim, 80, 100)
Linf_arr2  <- array(
  c(Linf_sim, Linf_sim * 1.1),        # 10% increase from 2010 onward
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
len <- Length(Pars = list(Linf = Linf_arr2,
                          K    = c(0.15, 0.25),
                          t0   = -0.1))

# ---- Direct array specification ----

## Supply MeanAtAge directly (Pars left empty).
## A plain numeric vector of length nAge is accepted; it is treated as a
## single simulation, single year and replicated by Extend() as needed.
ages   <- 0:20
len_aa <- Length(MeanAtAge = 100 * (1 - exp(-0.2 * (ages + 0.5))))

## Named Sim × Age × Year array — two change-point years, single simulation.
## Extend() replicates Sim = 1 to all nSim simulations automatically.
maa <- array(
  c(100 * (1 - exp(-0.2 * (ages + 0.5))),   # 1990 schedule
    110 * (1 - exp(-0.2 * (ages + 0.5)))),   # 2010 schedule (Linf increased)
  dim      = c(1, length(ages), 2),
  dimnames = list(Sim = 1, Age = ages, Year = c(1990, 2010))
)
len_arr <- Length(MeanAtAge = maa)

# ---- Custom length class midpoints ----

## By default, Classes and the ALK are populated automatically during
## Populate(). 
# Supply Classes explicitly to override the default bin width.
len_cls <- Length(
  Pars    = list(Linf = 100, K = 0.2, t0 = -0.1),
  Classes = seq(from = 2.5, by = 5, length.out = 26)
)

# ---- Slot accessors ----

len <- Length(Pars = list(Linf = 100, K = 0.2, t0 = -0.1))

## Read slots
MeanAtAge(len)            # NULL until Populate() is called
CVatAge(len)
Units(len)

## Replace slots
Units(len)   <- "cm"
CVatAge(len) <- 0.08
TruncSD(len) <- 3

# ---- Attaching to a Stock ----

stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
Length(stk) <- Length(Pars = list(Linf = 100, K = 0.2, t0 = -0.1))
Length(stk)

# ---- Populate ----

## The final populated object (typically done internally)
pop_len <- Populate(Length(stk))
pop_len

MeanAtAge(pop_len)