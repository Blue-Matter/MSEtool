library(salmonMSE)

SAR <- 0.01 # Marine survival
nsim <- 3 # Number of simulations
Bio <- new(
  "Bio",
  maxage = 3,
  p_mature = c(0, 0, 1),
  SRrel = "BH",
  capacity_smolt = 17250,           # Beverton-Holt asymptote. Not unfished capacity!
  kappa = 3,                        # Productivity in recruits per spawner
  Mjuv_NOS = c(0, -log(SAR), 0),    # Convert marine survival to an instantaneous mortality rate
  fec = c(0, 0, 5040),              # Spawning fecundity of NOS and HOS
  p_female = 0.49,
  s_enroute = 1
)

Habitat <- new(
  "Habitat",
  capacity_smolt_improve = 1,    # Keep carrying capacity unchanged
  kappa_improve = 1              # Keep productivity unchanged
)

Hatchery <- new(
  "Hatchery",
  n_yearling = 10000,             # Management lever. No hatchery if both this line and next line are zero
  n_subyearling = 0,              # Management lever. No hatchery if both this line and previous line are zero
  s_prespawn = 1,                 # Survival prior to spawning
  s_egg_smolt = 0.92,             # Survival of eggs in hatchery
  s_egg_subyearling = 1,
  Mjuv_HOS = Bio@Mjuv_NOS,
  gamma = 0.8,
  m = 1,                          # Mark rate of hatchery releases
  pmax_esc = 1,                   # Maximum proportion of escapement (after en route mortality) that could be used as broodtake
  pmax_NOB = 0.7,                 
  ptarget_NOB = 0.51,
  phatchery = 0.8,
  premove_HOS = 0,
  theta = c(100, 80),
  rel_loss = c(0.5, 0.4, 0.1),
  fec_brood = c(0, 0, 5040),
  fitness_type = c("Ford", "none"),
  zbar_start = c(93.1, 92),
  fitness_variance = 10,
  selection_strength = 3,
  heritability = 0.5,
  fitness_floor = 0.5
)

Harvest <- new(
  "Harvest",
  u_preterminal = 0,             # No pre-terminal fishery
  u_terminal = 0.203,            # Specify fixed harvest rate of mature fish
  MSF = FALSE,                   # No mark-selective fishing
  release_mort = c(0.1, 0.1),
  vulPT = c(0, 0, 0),
  vulT = c(1, 1, 1)
)

# Return of 1000 natural and hatchery fish each for the first generation
nyears <- 2
HistN <- array(0, c(nsim, Bio@maxage, nyears+1))
HistN[, 1, 1] <- HistN[, 2, 2] <- 1000/SAR

Historical <- new(
  "Historical",
  HistNjuv_NOS = HistN,
  HistNjuv_HOS = HistN
)

SOM <- new(
  "SOM",
  Bio, Habitat, Hatchery, Harvest, Historical,
  nsim = nsim, nyears = 2, proyears = 50
)

SOM <- check_SOM(SOM, silent = FALSE)

MOM <- SOM2MOM(SOM, check = TRUE)

# Convert 