# See FecundityModels() for all available built-in fecundity models and the
# parameter names required for each. All models use a logistic 50/95
# parameterisation with an asymptote of MaxFec.
FecundityModels()

# ---- Default behaviour (Fecundity omitted) ----

## When Fecundity is left empty, Populate() computes SProduction as mature
## weight-at-age (Weight@MeanAtAge * Maturity@MeanAtAge), so SProduction
## equals SBiomass. This is appropriate for most species.
stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
Fecundity(stk)    # empty fecundity-class object

# ---- Model-based specification — age-based ----

## FecundityAtAge: logistic fecundity-at-age
## A50     = age at 50% maximum fecundity
## A50_95  = age interval from 50% to 95% maximum fecundity
## MaxFec  = asymptotic (maximum) fecundity

## Constant parameters — same value for every simulation and year
fec <- Fecundity(Pars = list(A50 = 4, A50_95 = 2, MaxFec = 1e6))

## Stochastic A50 across simulations — drawn from Uniform(lower, upper)
fec <- Fecundity(Pars = list(A50    = c(3, 5),
                             A50_95 = 2,
                             MaxFec = 1e6))


## Time-varying MaxFec with named change-point arrays
## Only the years where values change need to be supplied; Extend() fills
## all intermediate and future years by forward-filling automatically.
nSim <- 48

mf_arr <- array(
  c(rep(1e6, nSim), rep(1.2e6, nSim)),   # MaxFec increases in 2010
  dim      = c(nSim, 2),
  dimnames = list(Sim = seq_len(nSim), Year = c(1990, 2010))
)
fec <- Fecundity(Pars = list(A50    = c(3, 5),
                             A50_95 = 2,
                             MaxFec = mf_arr))


# ---- Model-based specification — length-based ----

## FecundityAtLength: logistic fecundity-at-length
## L50     = length at 50% maximum fecundity
## L50_95  = length interval from 50% to 95% maximum fecundity
fec_l <- Fecundity(Pars = list(L50    = 40,
                               L50_95 = 8,
                               MaxFec = 1e6))

# ---- Model-based specification — weight-based ----

## FecundityAtWeight: logistic fecundity-at-weight
## W50     = weight at 50% maximum fecundity
## W50_95  = weight interval from 50% to 95% maximum fecundity
fec_w <- Fecundity(Pars = list(W50    = 2.0,
                               W50_95 = 0.5,
                               MaxFec = 1e6))

# ---- Direct array specification ----

## Supply MeanAtAge directly (Pars left empty).
## A plain numeric vector of length nAge is accepted; treated as a single
## simulation, single year and replicated by Extend() as needed.
ages   <- 0:20
fec_aa <- Fecundity(
  MeanAtAge = logistic_50_95(ages, x50 = 4, x50_95 = 2, asymp = 1e6)
)


## Named Sim x Age x Year array — two change-point years, single simulation.
## Extend() replicates Sim = 1 to all nSim simulations automatically.
maa <- array(
  c(logistic_50_95(ages, x50 = 4,   x50_95 = 2, asymp = 1e6),  # 1990
    logistic_50_95(ages, x50 = 4.5, x50_95 = 2, asymp = 1.2e6)),# 2010
  dim      = c(1, length(ages), 2),
  dimnames = list(Sim = 1, Age = ages, Year = c(1990, 2010))
)
fec_arr <- Fecundity(MeanAtAge = maa)

## Supply MeanAtLength directly — converted to MeanAtAge via the ALK
## during Populate() when a Length object is provided.
lens   <- seq(5, 120, by = 5)
fec_al <- Fecundity(
  MeanAtLength = array(
    logistic_50_95(lens, x50 = 40, x50_95 = 8, asymp = 1e6),
    dim      = c(1, length(lens), 1),
    dimnames = list(Sim = 1, Length = lens, Year = 1990)
  )
)

# ---- Slot accessors ----

fec <- Fecundity(Pars = list(L50 = 40, L50_95 = 8, MaxFec = 1e6))

## Read slots
MeanAtAge(fec)     # NULL until Populate() is called
MeanAtLength(fec)  # NULL unless an at-length model or direct array is used
Units(fec)

## Replace slots
Units(fec)         <- "eggs"
Classes(fec)       <- seq(5, 120, by = 5)

# ---- Attaching to a Stock ----

Fecundity(stk) <- Fecundity(Pars = list(L50 = 40, L50_95 = 8, MaxFec = 1e6))
Fecundity(stk)

# ---- Populate ----

## The final populated object (typically done internally).
## When Fecundity is populated, SProduction differs from SBiomass.
## At-length models require a populated Length object passed to Populate().
pop_fec <- Populate(Fecundity(stk),
                    Ages   = Ages(MaxAge = 20),
                    Length = Length(Pars = list(Linf = 100, K = 0.2, t0 = -0.1))
)
pop_fec

MeanAtLength(pop_fec)
MeanAtAge(pop_fec)
