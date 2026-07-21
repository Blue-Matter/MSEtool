# ---- Default behaviour (non-spatial models) ----

## For single-area models, leave Spatial empty.
## Populate() creates a default single-area structure automatically.
stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 5))
Spatial(stk)   # empty spatial-class object

# ---- Two-area model — fixed parameters ----

## UnfishedDist: fraction of unfished biomass in Area 1
## ProbStaying:  probability of remaining in Area 1
## RelativeSize: fraction of total area that is Area 1
## Area 2 values are derived automatically.
sp <- Spatial(UnfishedDist = 0.3,
              ProbStaying  = 0.6,
              RelativeSize = 0.4)
sp <- Populate(sp, Ages = Ages(MaxAge = 5), nSim = 1)
UnfishedDist(sp)   # asymptotic distribution: Sim x Area x Age x Year
Movement(sp)       # fitted movement matrix: Sim x FromArea x ToArea x Age x Year

# ---- Two-area model — stochastic across simulations ----

## Length-2 vectors are sampled from Uniform(lower, upper) once per simulation.
sp_stoch <- Spatial(UnfishedDist = c(0.2, 0.4),
                    ProbStaying  = c(0.5, 0.8),
                    RelativeSize = c(0.3, 0.5))
sp_stoch <- Populate(sp_stoch, Ages = Ages(MaxAge = 5), nSim = 48)
UnfishedDist(sp_stoch)   # nSim x 2 x Age x Year

# ---- Three-area model — fixed parameters ----

## FracOther is required for 3+ areas.
## Diagonal elements must be NA; off-diagonal [i,j] is the relative
## probability of moving from area i to area j (normalised internally).
nArea <- 3
ud <- matrix(c(0.5, 0.2, 0.3), nrow = 1, ncol = nArea)

fo <- array(NA, dim = c(1, nArea, nArea))
fo[1, 1, ] <- c(NA,  1,   0.1)   # from Area 1: 10x more likely to go to Area 2 than 3
fo[1, 2, ] <- c(1,   NA,  1  )   # from Area 2: equal probability to Area 1 and 3
fo[1, 3, ] <- c(0.1, 1,   NA )   # from Area 3: 10x more likely to go to Area 2 than 1

sp3 <- Spatial(UnfishedDist = ud,
               ProbStaying  = c(0.9, 0.2, 0.9),
               FracOther    = fo,
               RelativeSize = c(0.1, 0.4, 0.5))
sp3 <- Populate(sp3, Ages = Ages(MaxAge = 5), nSim = 1)
UnfishedDist(sp3)[1, , , 1]
Movement(sp3)[1, , , 1, 1]

# ---- Age-varying movement (3-area) ----

## Supply a named Sim x Area x Age array for UnfishedDist; movement is
## fitted independently for each age class.
nage <- nAge(Ages(MaxAge = 5))
ud3 <- array(NA, dim = c(1, nArea, nage))
ud3[, , 1] <- c(0.95, 0.045, 0.005)
ud3[, , 2] <- c(0.75, 0.20,  0.05 )
ud3[, , 3] <- c(0.50, 0.40,  0.10 )
ud3[, , 4] <- c(0.30, 0.50,  0.20 )
ud3[, , 5] <- c(0.10, 0.50,  0.40 )
ud3[, , 6] <- c(0.01, 0.20,  0.79 )

sp3_age <- Spatial(UnfishedDist = ud3, ProbStaying = 0.05, FracOther = fo)
sp3_age <- Populate(sp3_age, Ages = Ages(MaxAge = 5), nSim = 1)

## Asymptotic distribution and movement at youngest and oldest age class
UnfishedDist(sp3_age)[1, , , 1]
Movement(sp3_age)[1, , , 1,    1]   # youngest
Movement(sp3_age)[1, , , nage, 1]   # oldest

# ---- Supplying a movement matrix directly ----

## When Movement is supplied, UnfishedDist is derived from it.
## Each row (FromArea) must sum to 1 across ToAreas.
mov <- array(NA, dim = c(1, 2, 2),
             dimnames = list(Sim = 1, FromArea = 1:2, ToArea = 1:2))
mov[1, 1, ] <- c(0.7, 0.3)   # from Area 1: 70% stay, 30% move to Area 2
mov[1, 2, ] <- c(0.2, 0.8)   # from Area 2: 20% move to Area 1, 80% stay
sp_mov <- Spatial(Movement = mov)
sp_mov <- Populate(sp_mov, Ages = Ages(MaxAge = 5), nSim = 1)
UnfishedDist(sp_mov)   # derived from supplied Movement matrix

# ---- Equal density across areas ----

## RelativeSize = "EqualDensity" sets RelativeSize equal to mean
## UnfishedDist, producing constant density across areas.
sp_ed <- Spatial(UnfishedDist = 0.3,
                 ProbStaying  = 0.7,
                 RelativeSize = "EqualDensity")
sp_ed <- Populate(sp_ed, Ages = Ages(MaxAge = 5), nSim = 1)
RelativeSize(sp_ed)   # equals mean UnfishedDist

# ---- Slot accessors ----

## Read slots
CVDist(sp)
CVStay(sp)

## Replace slots
UnfishedDist(sp) <- 0.4
ProbStaying(sp)  <- 0.7
CVDist(sp)       <- 0.05

# ---- Attaching to a Stock ----

Spatial(stk) <- Spatial(UnfishedDist = 0.3,
                        ProbStaying  = 0.6,
                        RelativeSize = 0.4)
Spatial(stk)
