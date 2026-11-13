# Builds inst/extdata/ExampleData.csv, the example file for ReadDataCSV()
devtools::load_all()
set.seed(101)

Years  <- 2011:2025
nYear  <- length(Years)
Fleets <- c("Trawl", "Longline")
Ages   <- 0:10
Bins   <- seq(10, 80, by = 10)

Series <- function(mean, n = length(Fleets), nm = Fleets, miss = 0) {
  x <- matrix(round(mean * exp(rnorm(nYear * n, 0, 0.2)), 1), nYear, n,
              dimnames = list(Year = Years, Fleet = nm))
  if (miss) x[sample(length(x), miss)] <- NA
  x
}
Comp <- function(nClass, nSample = 200, keep = seq_len(nYear)) {
  x <- array(NA_real_, c(nYear, length(Fleets), nClass))
  p <- dnorm(seq_len(nClass), nClass / 2, nClass / 4)
  for (y in keep) for (f in seq_along(Fleets)) x[y, f, ] <- rmultinom(1, nSample, p)
  x
}

Landings <- Series(500)
Dat <- Data(
  Name = "Example data", CommonName = "Example fish", Species = "Piscis exemplaris",
  Agency = "Example agency", Region = "Example region", Latitude = -30, Longitude = 150,
  Years = Years,
  Landings = CatchData(Name = Fleets, Value = Landings, CV = Landings * 0 + 0.1, Units = "Biomass"),
  Discards = CatchData(Name = Fleets, Value = Series(50, miss = 4), Units = "Biomass"),
  Effort   = EffortData(Name = Fleets, Value = Series(100), CV = Series(0.2) * 0 + 0.2, Units = "Days"),
  CPUE     = IndicesData(Name = "Trawl CPUE", Value = Series(1, 1, "Trawl CPUE"),
                         CV = Series(1, 1, "Trawl CPUE") * 0 + 0.25, Units = "Biomass", Timing = 0.5),
  Survey   = IndicesData(Name = "Acoustic survey", Value = Series(2000, 1, "Acoustic survey", miss = 5),
                         Units = "Number", Timing = 0),
  LandingsAtAge  = CompData(Name = Fleets, Value = Comp(length(Ages), keep = 6:nYear),
                            Classes = Ages, Units = "years"),
  LandingsAtSize = CompData(Name = Fleets, Value = Comp(length(Bins), keep = 10:nYear),
                            Classes = list(Trawl = Bins, Longline = Bins), Units = "cm")
)

WriteDataCSV(Dat, "inst/extdata/ExampleData.csv", overwrite = TRUE)
