# Annual
array <- array(1:8,
  dim = c(2, 4),
  dimnames = list(
    Sim = 1:2,
    Year = c(1950, 1960, 1980, 2020)
  )
)
Years <- c(1940, 1955, 1965, 1985, 2025)
ExtendYears(array, Years)

# Seasonal
array <- array(1:8,
  dim = c(2, 4),
  dimnames = list(
    Sim = 1:2,
    Year = c(1950, 1950.25, 1950.5, 1950.75)
  )
)
Years <- c(1945.25, 1960, 1975.5)
ExtendYears(array, Years)
