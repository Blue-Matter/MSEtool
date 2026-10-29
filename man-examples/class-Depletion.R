# Default (Depletion omitted): stock starts unfished, terminal depletion
# determined by Fleet and Catchability parameters.
dep <- Depletion()

# Fixed initial depletion — same value for every simulation
dep <- Depletion(Initial = 0.8)

# Stochastic initial depletion — sampled from Uniform(0.6, 0.9) per simulation
dep <- Depletion(Initial = c(0.6, 0.9))

# Force terminal-year depletion to 0.4 relative to B0.
# Catchability Efficiency is optimised to achieve this.
dep <- Depletion(Final = 0.4)

# Both initial and final, relative to spawning biomass at unfished
# equilibrium (SB0) instead of the default total biomass (B0)
dep <- Depletion(Initial = 0.9, Final = 0.4, Reference = "SB0")

# Slot accessors
Initial(dep)                 # 0.9
Initial(dep) <- c(0.7, 0.9)   # now stochastic
Final(dep)   <- 0.3

# Attaching to a Stock (pass-through access)
stk <- Stock(Name = "Example stock", Ages = Ages(MaxAge = 20))
Depletion(stk) <- dep
Initial(Depletion(stk))

# After Populate(), Initial and Final become nSim-length arrays
pop_dep <- Populate(dep, nSim = 48)
Initial(pop_dep)
