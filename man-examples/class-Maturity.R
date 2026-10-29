# See MaturityModels() for all available built-in maturity models and the
# parameter names required for each.
MaturityModels()

# ---- Model-based specification ----

## Constant parameters (scalar) — same value for every simulation and year
mat <- Maturity(Pars = list(L50 = 40, L50_95 = 8))

## Stochastic across simulations — L50 drawn from Uniform(lower, upper)
## once per simulation; L50_95 fixed in all simulations
mat <- Maturity(Pars = list(L50    = c(35, 45),
                            L50_95 = 8))

## Inter-annual random walk on L50 — log-normal with 5% CV
## L50SD is removed from Pars after use
mat <- Maturity(Pars = list(L50    = c(35, 45),
                            L50SD  = 0.05,
                            L50_95 = 8))

## Time-varying parameters with named change-point arrays
## Only the years where values change need to be supplied; Extend() fills
## all intermediate and future years by forward-filling automatically.
nSim <- 48

## L50 shifts in 2010 (two change points: 1990 and 2010)
L50_arr <- array(
  c(rep(40, nSim), rep(45, nSim)),
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
mat <- Maturity(Pars = list(L50    = L50_arr,
                            L50_95 = 8))

# ---- Direct array specification ----

## Supply MeanAtAge directly (Pars left empty).
## A plain numeric vector of length nAge is accepted; treated as a single
## simulation, single year and replicated by Extend() as needed.
ages   <- 0:20
mat_aa <- Maturity(MeanAtAge = 1 / (1 + exp(-0.5 * (ages - 5))))

## Named Sim x Age x Year array — two change-point years, single simulation
maa <- array(
  c(1 / (1 + exp(-0.5 * (ages - 5))),    # 1990 ogive
    1 / (1 + exp(-0.5 * (ages - 7)))),   # 2010 ogive (L50 shifted later)
  dim      = c(1, length(ages), 2),
  dimnames = list(Sim = 1, Age = ages, Year = c(1990, 2010))
)
mat_arr <- Maturity(MeanAtAge = maa)

## Supply MeanAtLength directly — converted to MeanAtAge via the ALK
## during Populate() when a Length object is provided.
lens   <- seq(5, 120, by = 5)
mat_al <- Maturity(
  MeanAtLength = array(
    1 / (1 + exp(-log(19) * (lens - 40) / 8)),
    dim      = c(1, length(lens), 1),
    dimnames = list(Sim = 1, Length = lens, Year = 1990)
  )
)

# ---- Semelparity ----

## Semelparous = TRUE: post-spawn mortality equals the maturity ogive.
## Fully mature fish die with probability 1; fish on the ogive die
## proportionally; immature fish are unaffected.
## Appropriate for obligate semelparous species (e.g. Pacific salmon).
mat_sem <- Maturity(Pars = list(L50 = 40, L50_95 = 8), Semelparous = TRUE)

## Custom post-spawn mortality array — partial semelparity by age.
## Here mortality ramps from 0 at age 5 to 0.8 at age 15, then is
## constant. This differs from the maturity ogive and must be supplied
## as a named array.
sem_arr <- array(
  pmin(pmax((ages - 5) / 10, 0), 0.8),
  dim      = c(1, length(ages), 1),
  dimnames = list(Sim = 1, Age = ages, Year = 1990)
)
mat_psem <- Maturity(Pars = list(L50 = 40, L50_95 = 8), Semelparous = sem_arr)

## After Populate(), Semelparous is always an array — do not test == TRUE.
## Use any(Semelparous(mat) > 0) to check whether post-spawn mortality
## is active on a populated object.

# ---- Slot accessors ----

mat <- Maturity(Pars = list(L50 = 40, L50_95 = 8))

## Read slots
MeanAtAge(mat)      # NULL until Populate() is called
MeanAtLength(mat)   # NULL unless an at-length model or direct array is used
Semelparous(mat)    # scalar FALSE before Populate(); array after

## Replace slots
Semelparous(mat) <- TRUE
Classes(mat)     <- seq(0, 80, by = 5)

# ---- Attaching to a Stock ----

stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
Maturity(stk) <- Maturity(Pars = list(L50 = 40, L50_95 = 8))
Maturity(stk)

# ---- Populate ----

## The final populated object (typically done internally).
## At-length models require a Length object passed to Populate().
pop_mat <- Populate(Maturity(stk),
                    Ages   = Ages(MaxAge = 20),
                    Length = Length(Pars = list(Linf = 100, K = 0.2, t0 = -0.1))
)
pop_mat
