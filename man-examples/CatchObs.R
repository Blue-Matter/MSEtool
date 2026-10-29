# Uniform 20% CV — same CV used for all simulations
co <- CatchObs(CV = 0.2)

# Stochastic CV — unique CV per simulation drawn from lognormal(mean=0.2, sd=0.05)
co <- CatchObs(CV = c(0.2, 0.05))

# Bias as a CV — unique bias per simulation drawn from lognormal(mean=1, sd=0.1)
co <- CatchObs(CV = 0.2, Bias = 0.1)

# Pre-specified error array — bypasses CV entirely
err <- array(rlnorm(48 * 20), dim = c(48, 20),
             dimnames = list(Sim = 1:48, Year = 2001:2020))
co <- CatchObs(Error = err)

# Attach to an obs object
obs <- Obs(Landings = CatchObs(CV = 0.2),
           Discards = CatchObs(CV = 0.3, Bias = 0.05))
