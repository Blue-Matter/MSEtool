## ---- Default: zero discard mortality ----
# An empty DiscardMortality object causes PopulateDiscardMortality() to set
# discard mortality to 0 for all age and length classes (all discards survive).
# This is the default when no DiscardMortality object is supplied to Fleet().
dm_empty <- DiscardMortality()
dm_empty

## ---- Constant discard mortality (scalar) ----
# A scalar is applied to all age classes across all simulations and years.
# Here 30% of discarded fish die regardless of age.
dm_scalar <- DiscardMortality(MeanAtAge = 0.3)

## ---- Age-varying discard mortality (vector) ----
# A vector of length nAge specifies mortality per age class.
# Younger fish (smaller, more vulnerable) may have higher discard mortality.
ages    <- 0:20
dm_vals <- pmax(1 - ages / 10, 0)   # declines from 1 at age 0 to 0 at age 10

dm_vec <- DiscardMortality(MeanAtAge = dm_vals)

## ---- Full array (Sim x Age x Year) ----
# A full array allows discard mortality to vary across simulations and years.
dm_array <- array(
  0.2,
  dim      = c(1, length(ages), 1),
  dimnames = list(Sim = 1, Age = ages, Year = 2000)
)

dm_full <- DiscardMortality(MeanAtAge = dm_array)
MeanAtAge(dm_full)

## ---- Specifying discard mortality at length ----
# MeanAtLength takes precedence over MeanAtAge when both are supplied.
# MeanAtAge is derived from MeanAtLength via the age-length key during
# PopulateDiscardMortality().
lengths  <- seq(10, 100, by = 5)
dm_at_l  <- pmax(1 - lengths / 60, 0)   # mortality declines with length

dm_length_array <- array(
  dm_at_l,
  dim      = c(1, length(lengths), 1),
  dimnames = list(Sim = 1, Length = lengths, Year = 2000)
)

dm_len <- DiscardMortality(
  MeanAtLength = dm_length_array,
  Classes      = lengths
)
MeanAtLength(dm_len)

## ---- Attaching to a Fleet object ----
f <- Fleet(Name = "Trawl")
DiscardMortality(f) <- DiscardMortality(MeanAtAge = 0.2)
DiscardMortality(f)
MeanAtAge(DiscardMortality(f))

## ---- Extracting DiscardMortality from a Fleet ----
# Passing a fleet object as the first argument returns its DiscardMortality slot.
dm <- DiscardMortality(f)
dm
