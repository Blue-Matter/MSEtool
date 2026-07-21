# See SRRModels() for all available stock-recruitment models and their
# required parameters. Note: R0 always goes in its own slot, not in Pars.
SRRModels()

# ---- Beverton-Holt (default) ----

## Fixed steepness, deterministic recruitment
srr <- SRR(Pars = list(h = 0.7), R0 = 1000)

## Stochastic steepness — h drawn from Uniform(0.6, 0.9) once per simulation
srr <- SRR(Pars = list(h = c(0.6, 0.9)), R0 = 1000)

## Stochastic R0 across simulations
srr <- SRR(Pars = list(h = 0.7), R0 = c(500, 2000))

## Add recruitment process error: SD = 0.4, no autocorrelation
srr <- SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4)

## SD drawn from Uniform(0.3, 0.6), with lag-1 autocorrelation
srr <- SRR(Pars = list(h = 0.7), R0 = 1000,
           SD = c(0.3, 0.6), AC = 0.4)

# ---- Ricker ----

srr_r <- SRR(Pars  = list(hR = 0.7),
             Model = "Ricker",
             R0    = 1000,
             SD    = 0.4)

# ---- Hockey-stick ----

## Shinge is the hinge point relative to S0 (0 < Shinge <= 1)
srr_hs <- SRR(Pars  = list(Shinge = 0.3),
              Model = "HockeyStick",
              R0    = 1000,
              SD    = 0.4)

# ---- Spawning timing ----

## SpawnTimeFrac = 0 (default): spawning at start of step, no pre-spawn Z
## SpawnTimeFrac = 0.5:         mid-step spawning, exp(-0.5 * Z) applied first
## SpawnTimeFrac = 1:           end-of-step spawning, full exp(-Z) applied first
srr_mid <- SRR(Pars = list(h = 0.7), R0 = 1000, SpawnTimeFrac = 0.5)

# ---- Pre-specified recruitment deviations ----

## Supply RecDevHist directly to condition on observed recruitment.
## Dimensions: nSim x nHistTS. Values are multiplicative deviations
## in log-space (mean-zero lognormal deviates).
nSim    <- 48
nHistTS <- 50
hist_devs <- matrix(rnorm(nSim * nHistTS, 0, 0.4),
                    nrow = nSim, ncol = nHistTS)
dimnames(hist_devs) <- list(
  Sim  = seq_len(nSim),
  Year = seq(1970, by = 1, length.out = nHistTS)  
)
srr_cond <- SRR(Pars       = list(h = 0.7),
                R0         = 1000,
                SD         = 0.4,
                RecDevHist = hist_devs)

# ---- Multi-stock: SPFrom ----

## Stock 2 recruits based on stock 1's spawning production.
## Specify by 1-based index or by stock name.
srr_s2 <- SRR(Pars   = list(h = 0.7),
              R0     = 500,
              SPFrom = 1)          

# ---- Custom SRR model ----

## A custom model must accept S, S0, R0, and any named Pars.
## A matching RelRecFun with signature function(Pars, SPR) must also be given.
my_srr <- function(S, S0, R0, h) {
  4 * h * R0 * S / (S0 * (1 - h) + S * (5 * h - 1))
}
my_rrf <- function(Pars, SPR) {
  h  <- Pars$h
  CR <- 4 * h / (1 - h)
  pmax((CR * SPR - 1) / ((CR - 1) * SPR), 0)
}
srr_custom <- SRR(Pars       = list(h = 0.7),
                  Model      = my_srr,
                  R0         = 1000,
                  SD         = 0.4,
                  RelRecFun  = my_rrf)

# ---- Slot accessors ----

srr <- SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4, AC = 0.3)

## Read slots
Pars(srr)
R0(srr)
SD(srr)
AC(srr)
SpawnTimeFrac(srr)
RelRecFun(srr)      # NULL until Populate() is called for built-in models

## Replace slots
R0(srr)            <- 2000
SD(srr)            <- 0.5
SpawnTimeFrac(srr) <- 0.5

# ---- Attaching to a Stock ----

stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
SRR(stk) <- SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4)
SRR(stk)

# ---- Populate ----

## The final populated object (typically done internally).
## RecDevHist, RecDevProj, and RecDevInit are generated from SD and AC.
pop_srr <- Populate(SRR(stk), Ages = Ages(MaxAge = 20))
pop_srr
