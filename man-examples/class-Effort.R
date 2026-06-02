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

e_array_pop <- Populate(
  e_array,
  HistYears = hist_years,
  nSim      = nSim,
  nArea     = 3,
  seed      = 42
)
e_array_pop

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

e_df_pop <- Populate(
  e_df,
  HistYears = hist_years,
  nSim      = nSim,
  seed      = 42
)
e_df_pop

Effort(e_df_pop) |> head()


## ---- Spatial distribution across 3 areas ----
# Distribution must have dimensions Sim x Year x Area and sum to 1
# over the Area dimension. Here effort is fixed: 50% in area 1,
# 30% in area 2, and 20% in area 3 across all simulations and years.
# Supplying non-NA values fixes those cells; the internal allocation
# algorithm only fills NA cells.
dist_array <- array(
  c(0.5, 0.3, 0.2),
  dim      = c(1, 1, 3),
  dimnames = list(Sim = 1, Year = hist_years[1], Area = 1:3)
)

e_dist <- Effort(
  Effort       = effort_df,
  Distribution = dist_array,
  Units        = "hours"
)

e_dist_pop <- Populate(
  e_dist,
  HistYears = hist_years,
  nSim      = nSim,
  nArea     = 3,
  seed      = 42
)
Distribution(e_dist_pop)

## ---- Targeting: concentration of effort across areas ----
# Targeting (lambda) controls how strongly effort concentrates in
# high-utility areas via a softmax function.
# lambda = 0  : uniform distribution across accessible areas
# lambda = 0.8: moderate concentration (default)
# lambda = 3  : strongly directed effort toward highest-utility area

# Low targeting -- near-uniform allocation
e_low <- Effort(
  Effort    = effort_df,
  Targeting = array(0, dim = c(1, 1), dimnames = list(Sim = 1, Year = hist_years[1]))
)


# High targeting -- concentrated effort
e_high <- Effort(
  Effort    = effort_df,
  Targeting = array(3, dim = c(1, 1), dimnames = list(Sim = 1, Year = hist_years[1]))
)

## ---- Mode: Density vs Biomass ----
# Mode = "Density" (default) divides exploitable biomass by relative area
# size before computing utility, attracting fleets to high-concentration
# areas regardless of area size.
# Mode = "Biomass" uses raw exploitable biomass, making larger areas
# intrinsically more attractive.

e_density <- Effort(Effort = effort_df, Mode = "Density")
e_biomass <- Effort(Effort = effort_df, Mode = "Biomass")

## ---- Accessor and replacement functions ----
e <- Effort(Effort = effort_df, Units = "hours")

Mode(e)
Units(e)
Targeting(e)
Distribution(e)
Maximum(e)

# Modify individual slots
Mode(e) <- "Biomass"
Mode(e)

## ---- Extracting Effort from a Fleet object ----
f <- Fleet(Name = "Trawl", Effort = e)
Effort(f)
