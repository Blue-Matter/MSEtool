
# ---- Default behaviour (Depletion omitted) ----

## When Initial and Final are both NULL (the default), the object has no
## effect: stock starts unfished and terminal depletion is determined by
## the Fleet and Catchability parameters.
dep <- Depletion()
dep

# ---- Initial depletion ----

## Fixed initial depletion — same value for every simulation
dep <- Depletion(Initial = 0.8)

## Stochastic initial depletion — sampled from Uniform(0.6, 0.9)
## once per simulation
dep <- Depletion(Initial = c(0.6, 0.9))

## One value per simulation (nSim = 48)
nSim <- 48
dep  <- Depletion(Initial = runif(nSim, 0.6, 0.9))

# ---- Final depletion ----

## Force terminal-year depletion to 0.4 relative to B0.
## Catchability Efficiency is optimised to achieve this, overwriting any
## existing Efficiency values.
dep <- Depletion(Final = 0.4)

## Stochastic terminal depletion
dep <- Depletion(Final = c(0.3, 0.5))

# ---- Both initial and final ----

dep <- Depletion(Initial = 0.9, Final = 0.4)

# ---- Reference biomass ----

## Default: relative to total unfished biomass (B0)
dep <- Depletion(Final = 0.4, Reference = "B0")

## Relative to spawning biomass at unfished equilibrium (SB0)
dep <- Depletion(Final = 0.4, Reference = "SB0")

## "BMSY", "SBMSY", "SP0", and "SPMSY" pass validation but are reserved
## for future use and not currently implemented.

# ---- Slot accessors ----

dep <- Depletion(Initial = 0.8, Final = 0.4)

## Read slots
Initial(dep)    # 0.8 before Populate(); nSim-length array after
Final(dep)      # 0.4 before Populate(); nSim-length array after

## Replace slots
Initial(dep) <- c(0.7, 0.9)   # now stochastic
Final(dep)   <- 0.3

# ---- Attaching to a Stock ----

stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
Depletion(stk) <- Depletion(Initial = c(0.7, 0.8), 
                            Final = c(0.4, 0.6)
)
Depletion(stk)

## Pass-through access
Depletion(stk)           # returns stk@Depletion
Initial(Depletion(stk))  # initial depletion from the stock

# ---- Populate ----

## After Populate(), Initial and Final are nSim-length arrays (or NULL if
## not specified). Population is typically done internally.
pop_dep <- Populate(Depletion(stk), nSim = 48)
Initial(pop_dep)   # 48-element array
Final(pop_dep)     # 48-element array

