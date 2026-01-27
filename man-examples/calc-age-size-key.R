
nSim <- 5
nAge <- 15
nYear <- 2
ages <- 0:(nAge-1)
years <- 2001:2002

TruncSD <- 2
Dist <- 'normal'

set.seed(123)
Linf <- runif(nSim, min = 150, max = 200)
K <- 0.3      
t0 <- 0   

MeanAtAge <- array(0, 
                   dim = c(nSim, nAge, nYear),
                   dimnames = list(Sim = 1:nSim, Age = ages, Year = years))

for (sim in 1:nSim) {
  for (yr in 1:nYear) {
    MeanAtAge[sim, , yr] <- Linf[sim] * (1 - exp(-K * (ages - t0)))
  }
}

# Coefficient of variation (CV)
CVatAge <- array(0.1, dim = dim(MeanAtAge), dimnames = dimnames(MeanAtAge))

# Define size classes
SDatAge <- MeanAtAge * CVatAge
class_min <- floor(min(MeanAtAge - TruncSD * SDatAge))
class_max <- ceiling(max(MeanAtAge + TruncSD * SDatAge))

Classes <- seq(class_min, class_max, by = 10)


# Calculate Age-Size Key
ASK <- CalcAgeSizeKey(
  MeanAtAge = MeanAtAge,
  CVatAge = CVatAge,
  Classes = Classes,
  TruncSD = TruncSD,
  Dist = Dist
)

dim(ASK)


# Inspect first simulation, first year
ASK[1,,,1]

