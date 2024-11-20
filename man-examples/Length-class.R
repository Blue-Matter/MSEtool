LengthModels()

# von Bertalanffy growth model

## Constant over simulations and time
Length <- Length(Pars=list(Linf=100, K=0.2, t0=-0.1))

# Populate the object (usually done internally; see `?Populate`)
Length <- Populate(Length, Ages(20))
MeanAtAge(Length)

plot(Length)

## Uniform distributed over simulations, constant over time
Length <- Length(Pars=list(Linf=c(90,100),
                             K=c(0.15,0.2),
                             t0=-0.1))

# Log-normally distributed over time with 5% CV on Linf
Length <- Length(Pars=list(Linf=c(90,100),
                             LinfSD=0.05,
                             K=c(0.15,0.2),
                             t0=-0.1))

# Time-varying parameters
# Linf - uniformly distributed over simulations, normally distributed over time
nTS <- 30
nsim <- 10

Linf_mu <- runif(nsim, 50, 100)
Linf_sd <- 0.1 * Linf_mu

Length <- Length(Pars=list(Linf=matrix(rnorm(nTS,Linf_mu,Linf_sd),
                                       nsim, nTS),
                             K=c(0.15,0.2),
                             t0=-0.1))

# Change points in time-varying parameters
Length <- Length(Pars=list(
  Linf=matrix(rnorm(3, 100, 10),
              nrow=1, ncol=3), # single simulation, 3 time steps
  K=c(0.15,0.2), # uniform distribution
  t0=-0.1) # single value
  )

attributes(Length)$TimeSteps <- c(1980, 2000, 2020)


# Manually specify `MeanAtAge`, constant over sims and time

MeanAtAge <- vonBert(0:20, 100, 0.1, 0)

Length <- Length(MeanAtAge=MeanAtAge)

# Classes & Age-Size Key

Length <- Length(Pars=list(Linf=100, K=0.2, t0=-0.1),
                   Classes=seq(from=2.5, by=2.5, to=130))

plot(Length, Ages=Ages(20), type='ASK')

