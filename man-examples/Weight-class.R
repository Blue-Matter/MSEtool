WeightModels()

# Allometric age-weight model
Weight <- Weight(Pars=list(alpha=1E-3, beta=c(2.8, 3.2)))

# Populate the object (usually done internally; see `?Populate`)
Ages <- Ages(20)
Length <- Length(Pars=list(Linf=100, K=0.1, t0=0)) |>
  Populate(Ages)

Weight <- Populate(Weight, Ages, Length, nsim=2)

MeanAtAge(Weight)
MeanAtLength(Weight)

# Calculate `MeanAtLength` from `MeanAtAge` and `ASK(Length)`
Weight <- Populate(Weight, Ages, Length, CalcAtLength=TRUE, nsim=2)
MeanAtLength(Weight)

plot(Weight)


# Allometric length-weight model
Weight <- Weight(Pars=list(a=1E-3, b=c(2.8, 3.2)))

# Populate the object (usually done internally; see `?Populate`)
Ages <- Ages(20)
Length <- Length(Pars=list(Linf=100, K=0.1, t0=0)) |>
  Populate(Ages)

Weight <- Populate(Weight, Ages, Length, nsim=2)

MeanAtLength(Weight)
MeanAtAge(Weight)  # calculated from `MeanAtLength` and `ASK(Length)`

plot(Weight)

# see Examples in `?Length` for other `Pars` configurations

