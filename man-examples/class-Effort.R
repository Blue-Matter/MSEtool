## ---- Empty Effort object ----
# An empty Effort object is created by default when constructing a Fleet.
e <- Effort()
e

## ---- Effort from a pre-specified array ----
# A Sim x Year numeric array can be supplied directly.
# Year dimnames must match the historical years of the OM.
hist_years <- 2000:2024
nSim <- 48

effort_array <- array(
  seq(0.2, 1, length.out = length(hist_years)),
  dim = c(1, length(hist_years)),
  dimnames = list(Sim = 1, Year = hist_years)
)

e_array <- Effort(Effort = effort_array, Units = "hours")
e_array_pop <- Populate(e_array, HistYears = hist_years, nSim = nSim, nArea = 3, seed = 42)
Effort(e_array_pop)

## ---- Effort from a data frame (stochastic) ----
# A data frame of control points can be supplied to generate a stochastic
# Sim x Year effort array via GenHistEffort(). Effort is sampled uniformly
# between Lower and Upper at each control year, linearly interpolated to
# annual resolution, perturbed by lognormal process error (CV), and
# normalised to 1 in the terminal year.
effort_df <- data.frame(
  Year  = c(2000, 2008, 2016, 2024),
  Lower = c(0,    0.3,  0.7,  1),
  Upper = c(0,    0.5,  0.9,  1),
  CV    = c(0.1,  0.1,  0.1,  0.1)
)

e_df <- Effort(Effort = effort_df, Units = "hours")
e_df_pop <- Populate(e_df, HistYears = hist_years, nSim = nSim, seed = 42)
Effort(e_df_pop) |> head()

## ---- Spatial distribution across 3 areas ----
# Distribution must have dimensions Sim x Year x Area and sum to 1 over the
# Area dimension. Here effort is fixed: 50% in area 1, 30% in area 2, and
# 20% in area 3 across all simulations and years. Supplying non-NA values
# fixes those cells; the internal allocation algorithm only fills NA cells.
dist_array <- array(
  c(0.5, 0.3, 0.2),
  dim      = c(1, 1, 3),
  dimnames = list(Sim = 1, Year = hist_years[1], Area = 1:3)
)

e_dist <- Effort(Effort = effort_df, Distribution = dist_array, Units = "hours")
e_dist_pop <- Populate(e_dist, HistYears = hist_years, nSim = nSim, nArea = 3, seed = 42)
Distribution(e_dist_pop)

## ---- Targeting: how strongly effort follows the fish ----
# lambda = 0: effort spread evenly over space (equal F at equal density)
# lambda = 1: effort follows exploitable biomass (default)
# lambda = 3: effort concentrated where catch rates are highest
yr1 <- array(0, dim = c(1, 1), dimnames = list(Sim = 1, Year = hist_years[1]))

e_even  <- Effort(Effort = effort_df, Targeting = yr1)
e_high  <- Effort(Effort = effort_df, Targeting = yr1 + 3)

## ---- ReleaseValue: catch-and-release fleets ----
# 0 (default): only retained catch attracts effort
# 1: released catch attracts effort as much as retained catch
e_release <- Effort(Effort = effort_df, ReleaseValue = 1)

## ---- Mode: Density vs Biomass ----
# Mode = "Density" (default): F in an area is proportional to effort per unit
# area. Mode = "Biomass": each area is a discrete fishing ground and F is
# proportional to the effort share in that area.
e_density <- Effort(Effort = effort_df, Mode = "Density")
e_biomass <- Effort(Effort = effort_df, Mode = "Biomass")

## ---- Accessor and replacement functions ----
e <- Effort(Effort = effort_df, Units = "hours")

Mode(e)
Units(e)
Targeting(e)
ReleaseValue(e)
Distribution(e)
Maximum(e)

Mode(e) <- "Biomass"

## ---- Extracting Effort from a Fleet object ----
f <- Fleet(Name = "Trawl", Effort = e)
Effort(f)
