SRRModels()


# Beverton-Holt Model
SRR <- SRR(Pars=list(R0=1e5, h=0.6), SD=0.6, AC=0.1)

# Populate the object (usually done internally; see `?Populate`)
SRR <- Populate(SRR, MaxAge=10, nHistTS=20, nProjTS=5, nsim=2)

plot(SRR)

# see Examples in `?Length` for other `Pars` configurations
