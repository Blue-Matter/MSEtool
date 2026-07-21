## ---- List available retention models ----
RetentionModels()

## ---- Default: full retention ----
# An empty Retention object causes PopulateRetention() to set retention = 1
# for all age and length classes (all fish retained).
r_full <- Retention()
r_full

## ---- Double-normal retention-at-length (asymptotic) ----
# LR5: length at 5% retention; LFR: length at full retention;
# Rmaxlen = 1 produces asymptotic (full) retention above LFR.
r_asymptotic <- Retention(
  Pars = list(LR5 = 15, LFR = 28, Rmaxlen = 1)
)

## ---- Double-normal retention-at-length (dome-shaped) ----
# Rmaxlen < 1 allows retention to decline at larger lengths, representing
# a slot limit or size-based discard regulation.
r_dome <- Retention(
  Pars = list(LR5 = 15, LFR = 28, Rmaxlen = 0.4)
)

## ---- Logistic retention-at-length ----
# RL50: length at 50% retention; RL50_95: increment to 95% retention.
# MaxRet (default 1) caps the asymptote, allowing partial retention.
r_logistic <- Retention(
  Pars = list(RL50 = 25, RL50_95 = 8)
)

## ---- Logistic retention with partial maximum retention ----
# MaxRet < 1 models a fishery where large fish are only partially retained
# even at full selectivity (e.g. voluntary high-grading or regulatory caps).
r_partial <- Retention(
  Pars = list(RL50 = 25, RL50_95 = 8, MaxRet = 0.7)
)

## ---- Logistic retention-at-age ----
r_age <- Retention(
  Pars = list(RA50 = 3, RA50_95 = 2)
)

## ---- Knife-edge retention-at-length ----
# Full retention at or above RL; zero below (e.g. minimum size limit).
r_knife <- Retention(
  Pars = list(RL = 25)
)

## ---- Relative parameters (isRel = TRUE) ----
# When isRel = TRUE, LR5 and LFR are interpreted as multiples of the
# maturity L50 of the paired stock. A Maturity object must be available
# to PopulateRetention() for scaling to occur.
r_rel <- Retention(
  Pars  = list(LR5 = 0.4, LFR = 0.8, Rmaxlen = 1),
  isRel = TRUE
)

## ---- Direct MeanAtAge array ----
# When Pars is empty (default), MeanAtAge can be supplied directly.
# Values are preserved because Pars is empty.
ages      <- 0:20
ret_vals  <- ifelse(ages >= 3 & ages <= 12, 1, 0)  # slot limit: retain ages 3-12

ret_array <- array(
  ret_vals,
  dim      = c(1, length(ages), 1),
  dimnames = list(Sim = 1, Age = ages, Year = 2000)
)

r_direct <- Retention(MeanAtAge = ret_array)
MeanAtAge(r_direct)

## ---- Attaching to / extracting from a Fleet object ----
f <- Fleet(Name = "Trawl")
Retention(f) <- Retention(Pars = list(LR5 = 15, LFR = 28, Rmaxlen = 1))
Retention(f)

