# If `Movement` is provided, everything else (except RelativeSize) is calculated.
# Otherwise, `Movement` is calculated 


# Spatial Age Structure

# No Spatial Structure
Spatial <- Spatial() |> Populate()

UnfishedDist(Spatial) |> dimnames()
ProbStaying(Spatial) |> dimnames()
RelativeSize(Spatial) |> dimnames()
Movement(Spatial) |> dimnames()

# Two Area - deterministic
Spatial <- Spatial(UnfishedDist=0.3, # Fraction Unfished in Area 1
                   ProbStaying=0.6, # Prob Staying in Area 1 
                   RelativeSize=0.8)  |> # Relative Size of Area 1  
  Populate()

UnfishedDist(Spatial)
ProbStaying(Spatial)
RelativeSize(Spatial)
Movement(Spatial)

Spatial <- Spatial(Movement=Movement(Spatial))


# Two Area - Stochastic 
Spatial <- Spatial(UnfishedDist=c(0.095, 0.105),
                   ProbStaying=c(0.8, 0.9),
                   RelativeSize=c(0.095, 0.105)) |>
  Populate()

UnfishedDist(Spatial)
ProbStaying(Spatial)
RelativeSize(Spatial)
Movement(Spatial)[1,,,1,1]

# Equal Density
Spatial <- Spatial(UnfishedDist=c(0.095, 0.105),
                   ProbStaying=c(0.8, 0.9),
                   RelativeSize='EqualDensity') |>
  Populate()

UnfishedDist(Spatial)
RelativeSize(Spatial)


# Two Area - Movement by Age

Ages <- Ages(5)
nAge <- nAge(Ages)

# assume linear movement to Area 2 with age
UnfishedDist <- array(NA, dim=c(1, 2, nAge), 
                      dimnames = list(
                       Sim=1,
                       Area=1:2,
                       Age=Classes(Ages)
                      ))

UnfishedDist <- array(NA, dim=c(1, 2, nAge))

MoveTo2 <- seq(0.1, to=0.9, length.out=nAge)

for (a in 1:nAge) {
  UnfishedDist[,,a] <- c(1-MoveTo2[a], MoveTo2[a])
}

Spatial <- Spatial(UnfishedDist=UnfishedDist,
                   ProbStaying=0.95) |>
  Populate()


UnfishedDist(Spatial)[1,,,1]
Movement(Spatial)[1,,,1,1] 
Movement(Spatial)[1,,,6,1] 


# Three Area Model 
nArea <- 3
UnfishedDist <- matrix(c(0.5, 0.2, 0.3), nrow=1, ncol=nArea)

# the relative fraction moving from one area to the others
FracOther <- array(NA, dim=c(1, nArea, nArea))
FracOther[1,1,] <- c(NA, 1, 0.1) # movement in to Area 3 is 10% of movement into Area 2
FracOther[1,2,] <- c(1, NA, 1)
FracOther[1,3,] <- c(0.1, 1, NA) # movement in to Area 1 is 10% of movement into Area 2

Spatial <- Spatial(UnfishedDist=UnfishedDist,
                   ProbStaying=c(0.9, 0.2, 0.9), # initial values for prob. staying
                   FracOther=FracOther,
                   RelativeSize=c(0.1, 0.4, 0.5)) |>
  Populate()

UnfishedDist(Spatial)[1,,,]
Movement(Spatial)[1,,,,]
RelativeSize(Spatial)


# Three Area -  movement by Age
nArea <- 3
Ages <- Ages(5)
nAge <- nAge(Ages)

# Movement from Area 1 to Area 3 with increasing age
UnfishedDist <- array(NA, dim=c(1, nArea, nAge))
UnfishedDist[,,1] <- c(0.95, 0.045, 0.005)
UnfishedDist[,,2] <- c(0.75, 0.20, 0.05)
UnfishedDist[,,3] <- c(0.5, 0.4, 0.1)
UnfishedDist[,,4] <- c(0.3, 0.5, 0.2)
UnfishedDist[,,5] <- c(0.1, 0.5, 0.4)
UnfishedDist[,,6] <- c(0.01, 0.2, 0.79)

# the relative fraction moving from one area to the others
FracOther <- array(NA, dim=c(1, nArea, nArea))
FracOther[1,1,] <- c(NA, 1, 0.1) # movement in to Area 3 is 10% of movement into Area 2
FracOther[1,2,] <- c(1, NA, 1)
FracOther[1,3,] <- c(0.1, 1, NA) # movement in to Area 1 is 10% of movement into Area 2

Spatial <- Spatial(UnfishedDist=UnfishedDist,
                   ProbStaying=0.05,
                   FracOther=FracOther) |>
  Populate()

UnfishedDist(Spatial)[1,,,]
Movement(Spatial)[1,,,,]


# Pass Movement Matrix Directly ...
Spatial <- Spatial(Movement=Movement(Spatial)) |>
  Populate()


# Asymptotic Distribution
UnfishedDist(Spatial)[1,,,]

# Movement matrix - Age 0
Movement(Spatial)[1,,,1,]

# Movement matrix - Age 5
Movement(Spatial)[1,,,6,]




# Seasonal-Spatial Age Structure 

# Seasonal Migration ...





