# ExtendSims
array <- array(c(0.5, 0.1),
               dim = c(1,2),
               dimnames = list(Sim = 1, Year=2025:2026)
)
Extend(array, nSim=5)
ExtendSims(array, 5)

# ExtendAges 
array <- array(c(0.5, 0.1),
               dim = c(2,1,2),
               dimnames = list(Sim = 1:2, Age=0, Year=2025:2026)
)
Extend(array, AgeClasses = 0:10)
ExtendAges(array, 0:10)


# ExtendYears

## Annual
array <- array(1:8,
  dim = c(2, 4),
  dimnames = list(
    Sim = 1:2,
    Year = c(1950, 1960, 1980, 2020)
  )
)
Years <- c(1940, 1955, 1965, 1985, 2025)
Extend(array, Years=Years)
ExtendYears(array, Years)

## Seasonal
array <- array(1:8,
  dim = c(2, 4),
  dimnames = list(
    Sim = 1:2,
    Year = c(1950, 1950.25, 1950.5, 1950.75)
  )
)
Years <- c(1945.25, 1960, 1975.5)
ExtendYears(array, Years)

