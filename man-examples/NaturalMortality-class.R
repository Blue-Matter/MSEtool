
NaturalMortalityModels()

# Constant M-at-Age
NaturalMortality <- NaturalMortality(Pars=list(M=0.2))

# Populate the object (usually done internally; see `?Populate`)
Ages <- Ages(20)
Length <-  Length(Pars=list(Linf=100, K=0.1, t0=0)) |>  Populate(Ages)

NaturalMortality <- Populate(NaturalMortality, Ages, Length)

plot(NaturalMortality)


# M-at-Length - Lorenzen Model
NaturalMortality <- NaturalMortality(Pars=list(M=0.2,
                                               RefLength=100,
                                               c=-1))

NaturalMortality <- Populate(NaturalMortality, Ages, Length)

plot(NaturalMortality)

# see Examples in `?Length` for other `Pars` configurations


