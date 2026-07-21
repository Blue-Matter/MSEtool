## ---- List available selectivity models ----
SelectivityModels()

## ---- Double-normal selectivity-at-length (asymptotic) ----
# The double-normal model is parameterised by L5 (length at 5% selectivity),
# LFS (length at full selectivity), and Vmaxlen (selectivity at max length).
# Vmaxlen = 1 produces an asymptotic curve; values < 1 produce dome-shaped.
s_asymptotic <- Selectivity(
  Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1)
)
isRel(s_asymptotic)

## ---- Double-normal selectivity-at-length (dome-shaped) ----
s_dome <- Selectivity(
  Pars = list(L5 = 20, LFS = 35, Vmaxlen = 0.5)
)

## ---- Logistic selectivity-at-length ----
# Parameterised by SL50 (length at 50% selectivity) and SL50_95 (the
# length increment from 50% to 95% selectivity).
s_logistic <- Selectivity(
  Pars = list(SL50 = 30, SL50_95 = 10)
)

## ---- Logistic selectivity-at-age ----
s_age <- Selectivity(
  Pars = list(SA50 = 3, SA50_95 = 2)
)

## ---- Knife-edge selectivity-at-length ----
# Full selectivity at or above SL; zero below.
s_knife <- Selectivity(
  Pars = list(SL = 25)
)

## ---- Relative parameters (isRel = TRUE) ----
# When isRel = TRUE, L5 and LFS are interpreted as multiples of the
# maturity L50 of the paired stock. This is useful when applying the same
# fleet configuration across stocks with different growth characteristics.
# PopulateSelectivity() scales L5 and LFS by L50 before computing the curve;
# a Maturity object must be available at population time.
s_rel <- Selectivity(
  Pars  = list(L5 = 0.5, LFS = 0.9, Vmaxlen = 1),
  isRel = TRUE
)
isRel(s_rel)

## ---- Direct MeanAtAge array ----
# When Pars is empty (default), MeanAtAge can be supplied directly.
# The array must have dimensions Sim x Age x Year with named dimnames.
# Any values in MeanAtAge are preserved because Pars is empty.
ages     <- 0:20
sel_vals <- pmin(ages / 5, 1)   # ramp from 0 to 1, plateau at age 5

sel_array <- array(
  sel_vals,
  dim      = c(1, length(ages), 1),
  dimnames = list(Sim = 1, Age = ages, Year = 2000)
)

s_direct <- Selectivity(MeanAtAge = sel_array)
MeanAtAge(s_direct)

## ---- isRel accessor and replacement ----
s <- Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1))
isRel(s)
isRel(s) <- TRUE
isRel(s)

## ---- Attaching to / extracting from a Fleet object ----
# Passing a fleet object as the first argument returns its Selectivity slot.
f <- Fleet(Name = "Trawl")
Selectivity(f) <- Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1))
Selectivity(f)
isRel(Selectivity(f))

