MaturityModels()

# Logistic Maturity-at-Age
Maturity <- Maturity(Pars=list(A50=2, A50_95=0.5))

# Populate the object (usually done internally; see `?Populate`)
Ages <- Ages(10)
Length <-  Length(Pars=list(Linf=100, K=0.1, t0=0)) |>  Populate(Ages)

Maturity <- Populate(Maturity,
                     Ages,
                     Length)

MeanAtAge(Maturity)
MeanAtLength(Maturity)

# Calculate `MeanAtLength` from `MeanAtAge` and `ASK(Length)`
Maturity <- Populate(Maturity, Ages, Length, CalcAtLength=TRUE, nsim=2)
MeanAtLength(Maturity)

plot(Maturity)

# Logistic Maturity-at-Length
Maturity <- Maturity(Pars=list(L50=50, L50_95=10))

# Populate the object (usually done internally; see `?Populate`)
Maturity <- Populate(Maturity, Ages, Length)

MeanAtLength(Maturity)
MeanAtAge(Maturity)  # calculated from `MeanAtLength` and `ASK(Length)`

plot(Maturity)

# see Examples in `?Length` for other `Pars` configurations
