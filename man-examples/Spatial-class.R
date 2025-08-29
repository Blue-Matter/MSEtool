

# Two Area
Spatial <- Spatial(UnfishedDist=0.3, # Fraction Unfished in Area 1
                   ProbStaying=0.6,
                   RelativeSize=0.8)

# Populate the object (usually done internally; see `?Populate`)
Spatial <- Populate(Spatial)

UnfishedDist(Spatial) # Spatial distribution
Movement(Spatial) # Movement matrix
RelativeSize(Spatial)

# Two Area - stochastic
Spatial <- Spatial(UnfishedDist=c(0.1, 0.3),
                   ProbStaying=c(0.8, 0.9),
                   RelativeSize=c(0.2,0.4))

# Populate the object (usually done internally; see `?Populate`)
Spatial <- Populate(Spatial, nsim=5)

plot(Spatial)

UnfishedDist(Spatial)
Movement(Spatial)
RelativeSize(Spatial)


# Two Area - Movement by Age
nsim <- 1
Ages <- Ages(5)
nage <- nAge(Ages)

# assume linear movement to Area 2 with age
UnfishedDist <- array(NA, dim=c(nsim, 2, nage))
MoveTo2 <- seq(0.1, to=0.9, length.out=nage)
for (a in 1:nage) {
  UnfishedDist[,,a] <- c(1-MoveTo2[a], MoveTo2[a])
}

Spatial <- Spatial(UnfishedDist=UnfishedDist,
                   ProbStaying=0.95)

Spatial <- Populate(Spatial, nsim=nsim)


plot(Spatial)

# Asymptotic Distribution
UnfishedDist(Spatial)[1,,,1]

# Movement matrix - Age - 0
Movement(Spatial)[1,,,1,]

# Movement matrix - Age 5
Movement(Spatial)[1,,,6,]

# Three Area Model
narea <- 3
UnfishedDist <- matrix(c(0.5, 0.2, 0.3), nrow=1, ncol=narea)


# the relative fraction moving from one area to the others
FracOther <- array(NA, dim=c(1, narea, narea))
FracOther[1,1,] <- c(NA, 1, 0.1) # movement in to Area 3 is 10% of movement into Area 2
FracOther[1,2,] <- c(1, NA, 1)
FracOther[1,3,] <- c(0.1, 1, NA) # movement in to Area 1 is 10% of movement into Area 2

Spatial <- Spatial(UnfishedDist=UnfishedDist,
                   ProbStaying=c(0.9, 0.2,0.9), # initial values for prob. staying
                   FracOther=FracOther,
                   RelativeSize=c(0.1, 0.4, 0.5))

Spatial <- Populate(Spatial)

UnfishedDist(Spatial)[1,,,]
Movement(Spatial)[1,,,,]
RelativeSize(Spatial)



# Three Area movement by Age
nsim <- 1
narea <- 3
nage <- nAge(Ages(5))

# Movement from Area 1 to Area 3 with increasing age
UnfishedDist <- array(NA, dim=c(nsim, narea, nage))
UnfishedDist[,,1] <- c(0.95, 0.045, 0.005)
UnfishedDist[,,2] <- c(0.75, 0.20, 0.05)
UnfishedDist[,,3] <- c(0.5, 0.4, 0.1)
UnfishedDist[,,4] <- c(0.3, 0.5, 0.2)
UnfishedDist[,,5] <- c(0.1, 0.5, 0.4)
UnfishedDist[,,6] <- c(0.01, 0.2, 0.79)

# the relative fraction moving from one area to the others
FracOther <- array(NA, dim=c(nsim, narea, narea))
FracOther[1,1,] <- c(NA, 1, 0.1) # movement in to Area 3 is 10% of movement into Area 2
FracOther[1,2,] <- c(1, NA, 1)
FracOther[1,3,] <- c(0.1, 1, NA) # movement in to Area 1 is 10% of movement into Area 2

Spatial <- Spatial(UnfishedDist=UnfishedDist,
                          ProbStaying=0.05,
                          FracOther=FracOther)

Spatial <- Populate(Spatial)

# Asymptotic Distribution
UnfishedDist(Spatial)[1,,,]

# Movement matrix - Age 0
Movement(Spatial)[1,,,1,]

# Movement matrix - Age 5
Movement(Spatial)[1,,,6,]




