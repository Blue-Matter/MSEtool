
FecundityModels()

# Fecundity-at-Age-Model
Fecundity <- Fecundity(Pars=list(A50=3, A50_95=4, MaxFec=1E6))
Model(Fecundity)

# Populate the object (usually done internally; see `?Populate`)
Fecundity <- Populate(Fecundity, Ages=Ages(20))

plot(Fecundity)

# Fecundity-at-Length-Model
Fecundity <- Fecundity(Pars=list(L50=50, L50_95=10, MaxFec=1E6))
Model(Fecundity)

# Populate the object (usually done internally; see `?Populate`)
Fecundity <- Populate(Fecundity,
                      Ages=Ages(20),
                      Length=Length(Pars=list(Linf=100, K=0.1, t0=0)))


plot(Fecundity)

# see Examples in `?Length` for other `Pars` configurations
