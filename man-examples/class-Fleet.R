## ---- Minimal Fleet object ----
# Effort and Selectivity are required. All other components use defaults:
# Catchability = 1, Retention = full, DiscardMortality = 0,
# Closure = all open, WeightFleet = stock weight-at-age.
eff <- Effort(Effort = data.frame(
  Year  = c(2000, 2010, 2024),
  Lower = c(0,    0.5,  1),
  Upper = c(0,    0.7,  1),
  CV    = c(0.1,  0.1,  0.1)
))
sel <- Selectivity(Pars = list(L5 = 20, LFS = 35, Vmaxlen = 1))

f <- Fleet(Name = "Trawl", Effort = eff, Selectivity = sel)
f

## ---- Fleet with all components specified ----
f_full <- Fleet(
  Name             = "Trawl",
  Effort           = eff,
  Catchability     = Catchability(Efficiency = 0.01),
  Selectivity      = sel,
  Retention        = Retention(Pars = list(LR5 = 15, LFR = 28, Rmaxlen = 1)),
  DiscardMortality = DiscardMortality(MeanAtAge = 0.2)
)
f_full

## ---- Closure: partial spatio-temporal closure ----
# Closure has dimensions Sim x Year x Area (Sim/Year may be length 1 and are
# replicated internally). 1 = open, 0 = closed.
# Here area 3 is closed from 2010 onwards; areas 1 and 2 are always open.
years <- 2000:2024
closure_array <- array(1, dim = c(1, length(years), 3),
                       dimnames = list(Sim = 1, Year = years, Area = 1:3))
closure_array[1, as.character(2010:2024), 3] <- 0

f_closure <- Fleet(Name = "Trawl", Effort = eff, Selectivity = sel,
                    Closure = closure_array)
Closure(f_closure)

## ---- Slot accessors and replacement ----
Effort(f_full)
Catchability(f_full)
Selectivity(f_full)
Retention(f_full)
DiscardMortality(f_full)
WeightFleet(f_full)

Selectivity(f_full)   <- Selectivity(Pars = list(L5 = 25, LFS = 40, Vmaxlen = 0.8))
Catchability(f_full)  <- Catchability(Efficiency = 0.02)

## ---- Assigning fleets to an OM ----
# Stock objects must be added to the OM before fleets.
stock1 <- Stock(Name = "Snapper")
stock2 <- Stock(Name = "Grouper")
om <- OM()
Stock(om) <- list(Snapper = stock1, Grouper = stock2)

# A single Fleet applied to all stocks by name
Fleet(om) <- f
Fleet(om)

# A stock-indexed list of fleets: stock -> fleet -> Fleet object
longline <- Fleet(Name = "Longline", Effort = eff,
                   Selectivity = Selectivity(Pars = list(SA50 = 4, SA50_95 = 2)))
Fleet(om) <- list(Snapper = list(Trawl = f), Grouper = list(Longline = longline))
Fleet(om)

## ---- Pass-through access from an OM ----
Fleet(om)        # full fleet list
Fleet(om, 1)     # all fleets for stock index 1 (Snapper)
Fleet(om, 2, 1)  # fleet index 1 for stock index 2 (Grouper -> Longline)
