## ---- Minimal Fleet object ----
# Effort and Selectivity are required. All other components use defaults:
# Catchability = 1, Retention = full, DiscardMortality = 0,
# Closure = all open, WeightFleet = stock weight-at-age.
f <- Fleet(
  Name        = "Trawl",
  Effort      = Effort(
    Effort = data.frame(
      Year  = c(2000, 2010, 2024),
      Lower = c(0,    0.5,  1),
      Upper = c(0,    0.7,  1),
      CV    = c(0.1,  0.1,  0.1)
    )
  ),
  Selectivity = Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1))
)
f


## ---- Fleet with all components specified ----
f_full <- Fleet(
  Name             = "Trawl",
  Effort           = Effort(
    Effort = data.frame(
      Year  = c(2000, 2010, 2024),
      Lower = c(0,    0.5,  1),
      Upper = c(0,    0.7,  1),
      CV    = c(0.1,  0.1,  0.1)
    ),
    Units = "hours"
  ),
  Catchability     = Catchability(Efficiency = 0.01),
  Selectivity      = Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1)),
  Retention        = Retention(Pars = list(LR5 = 15, LFR = 28, Rmaxlen = 1)),
  DiscardMortality = DiscardMortality(MeanAtAge = 0.2)
)
f_full


## ---- Closure: partial spatio-temporal closure ----
# Closure has dimensions Sim x Year x Area.
# Sim and Year dimensions may be length 1 (replicated internally).
# 1 = open, 0 = closed.
# Here area 3 is closed from 2010 onwards; areas 1 and 2 are always open.
years <- 2000:2024

closure_array <- array(
  1,
  dim      = c(1, length(years), 3),
  dimnames = list(Sim = 1, Year = years, Area = 1:3)
)

# Close area 3 from 2010 onwards
closure_array[1, as.character(2010:2024), 3] <- 0

f_closure <- Fleet(
  Name        = "Trawl",
  Effort      = Effort(
    Effort = data.frame(
      Year  = c(2000, 2010, 2024),
      Lower = c(0,    0.5,  1),
      Upper = c(0,    0.7,  1),
      CV    = c(0.1,  0.1,  0.1)
    )
  ),
  Selectivity = Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1)),
  Closure     = closure_array
)
Closure(f_closure)

## ---- Slot accessors and replacement ----
f <- Fleet(
  Name             = "Trawl",
  Effort           = Effort(
    Effort = data.frame(
      Year  = c(2000, 2010, 2024),
      Lower = c(0,    0.5,  1),
      Upper = c(0,    0.7,  1),
      CV    = c(0.1,  0.1,  0.1)
    )
  ),
  Catchability     = Catchability(Efficiency = 0.01),
  Selectivity      = Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1)),
  Retention        = Retention(Pars = list(LR5 = 15, LFR = 28, Rmaxlen = 1)),
  DiscardMortality = DiscardMortality(MeanAtAge = 0.2)
)

Effort(f)
Catchability(f)
Selectivity(f)
Retention(f)
DiscardMortality(f)
Closure(f)
WeightFleet(f)

# Replace individual slots
Selectivity(f) <- Selectivity(Pars = list(L5 = 25, LFS = 40, Vmaxlen = 0.8))
Selectivity(f)

Catchability(f) <- Catchability(Efficiency = 0.02)
Efficiency(Catchability(f))

## ---- Assigning a single Fleet to all stocks in an OM ----
# Stock objects must be added to the OM before fleets.
# A single Fleet object is applied to all stocks by name.
stock1 <- Stock(Name = "Snapper")
stock2 <- Stock(Name = "Grouper")

om <- OM()
Stock(om) <- list(Snapper = stock1, Grouper = stock2)

trawl <- Fleet(
  Name        = "Trawl",
  Effort      = Effort(
    Effort = data.frame(
      Year  = c(2000, 2010, 2024),
      Lower = c(0,    0.5,  1),
      Upper = c(0,    0.7,  1),
      CV    = c(0.1,  0.1,  0.1)
    )
  ),
  Selectivity = Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1))
)

Fleet(om) <- trawl   # applied to all stocks
Fleet(om)

## ---- Assigning a stock-indexed list of fleets to an OM ----
# A list structured as stock -> fleet -> Fleet object allows different
# fleets to be assigned per stock.
longline <- Fleet(
  Name        = "Longline",
  Effort      = Effort(
    Effort = data.frame(
      Year  = c(2000, 2010, 2024),
      Lower = c(0,    0.3,  1),
      Upper = c(0,    0.5,  1),
      CV    = c(0.1,  0.1,  0.1)
    )
  ),
  Selectivity = Selectivity(Pars = list(SA50 = 4, SA50_95 = 2))
)

fleet_list <- list(
  Snapper = list(Trawl    = trawl),
  Grouper = list(Longline = longline)
)

Fleet(om) <- fleet_list
Fleet(om)

## ---- Pass-through access from an OM ----
# When the first argument is an om object, Fleet() acts as an accessor.

# Return the full fleet list
Fleet(om)

# Return all fleets for stock index 1 (Snapper)
Fleet(om, 1)

# Return fleet index 1 for stock index 2 (Grouper -> Longline)
Fleet(om, 2, 1)
