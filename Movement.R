devtools::load_all()

# Two Area
Spatial <- Spatial(UnfishedDist=0.3, # Fraction Unfished in Area 1
                   ProbStaying=0.6,
                   RelativeSize=0.8)

# Populate the object (usually done internally; see `?Populate`)
Spatial <- Populate(Spatial)

UnfishedDist(Spatial) # Spatial distribution
Movement(Spatial)[1,,,1,1] # Movement matrix
CalcAsymptoticDist(Movement(Spatial)[1,,,1,1])




# Test Movement and Asymptotic Distribution

nareas <- 2
N1 <- matrix(UnfishedDist(Spatial)[1,,1,1], nrow=4, ncol=nareas, byrow=TRUE)
nage <- nrow(N1)

mov <- replicate(nage, Movement(Spatial)[1,,,1,1]) |> aperm(c(3,1,2))

movestockCPP(nareas, nage-1, mov, N1)

# Two Area by Age 
UnfishedDist <- array(NA, c(1, nareas, nage))
UnfishedDist[1,,1] <- c(0.3, 0.7)
UnfishedDist[1,,2] <- c(0.8, 0.2)
UnfishedDist[1,,3] <- c(0.5, 0.5)
UnfishedDist[1,,4] <- c(0.6, 0.4)

Spatial <- Spatial(UnfishedDist=UnfishedDist, # Fraction Unfished in Area 1
                   ProbStaying=0.9,
                   RelativeSize=0.8)

Spatial <- Populate(Spatial)

mov <- Movement(Spatial)[1,,,,1] |> aperm(c(3,1,2))

Nnext <- N1
for (i in 1:100) 
  Nnext <- movestockCPP(nareas, nage-1, mov, Nnext)

Nnext


nareas <- 3
nage <- 5
nts <- 3
spat <- Spatial()
spat@UnfishedDist <- array(NA, c(1, nareas, nage, nts))
spat@UnfishedDist[1,,,1] <- matrix(c(0.6, 0.3, 0.1), nrow=nage, ncol=nts, byrow=TRUE)
spat@UnfishedDist[1,,,2] <- matrix(c(0.4, 0.5, 0.1), nrow=nage, ncol=nts, byrow=TRUE) 
spat@UnfishedDist[1,,,3] <- matrix(c(0.2, 0.3, 0.5), nrow=nage, ncol=nts, byrow=TRUE) 
spat@ProbStaying <- 0.9
FracOther <- array(NA, c(1, nareas, nareas))
FracOther[1,1,] <- c(NA, 1, 0.1)
FracOther[1,2,] <- c(1, NA, 0.4)
FracOther[1,3,] <- c(0.1, 0.9, NA)

spat@FracOther <- FracOther
spat <- CalcMovement(spat)

spat@Movement[1,,,1,1]
spat@Movement[1,,,2,1]

CalcAsymptoticDist(spat@Movement[1,,,3,1])


