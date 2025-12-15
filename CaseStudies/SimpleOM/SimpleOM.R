library(MSEtool)











## ---- CV-length-uniform ----
Length_CV_uniform <- Length(CVatAge = c(0.1, 0.2))

## ---- CV-length-uniform-populate----

Length_CV_uniform <- Populate(Length_CV_uniform,
  Ages = Ages(ExampleStock),
  Years = Years(ExampleStock),
  nSim = nSim(ExampleOM),
  seed = 101
)

## ---- CV-length-uniform-dim ----
Length_CV_uniform |>
  CVatAge() |>
  dim()

## ---- CV-length-uniform-dimnames ----
Length_CV_uniform |>
  CVatAge() |>
  dimnames()

## ---- CV-length-uniform-head ----
Length_CV_uniform |>
  CVatAge()


## ---- CV-length-other ----

# numeric length nSim
set.seed(101)
CVatAge <- runif(nSim(ExampleOM), 0.1, 0.2)

Length_CV_uniform_1 <- Length(CVatAge = CVatAge) |>
  Populate(
    Ages = Ages(ExampleStock),
    Years = Years(ExampleStock),
    nSim = nSim(ExampleOM)
  )

Length_CV_uniform_1 |>
  CVatAge()


## ---- CV-length-other-dist ----

# Sample from log-normal distribution
set.seed(101)
CVatAge <- trlnorm(nSim(ExampleOM), 0.15, 0.2)

Length_CV_lognorm <- Length(CVatAge = CVatAge) |>
  Populate(
    Ages = Ages(ExampleStock),
    Years = Years(ExampleStock),
    nSim = nSim(ExampleOM)
  )

Length_CV_lognorm |>
  CVatAge()

## ---- CV-length-age-specific ----

set.seed(101)
CVatAge <- array(
  runif(
    nSim(ExampleOM) *
      nAge(ExampleStock),
    0.1, 0.2
  ),
  dim = c(
    nSim(ExampleOM),
    nAge(ExampleStock)
  )
)

Length_CV_uniform_Age <- Length(CVatAge = CVatAge) |>
  Populate(
    Ages = Ages(ExampleStock),
    Years = Years(ExampleStock),
    nSim = nSim(ExampleOM)
  )

Length_CV_uniform_Age |>
  CVatAge() |>
  dimnames()

# ---- CV-length-age-time ----

set.seed(101)
CVatAge <- array(
  runif(
    nSim(ExampleOM) *
      nAge(ExampleStock) *
      length(Years(ExampleOM)),
    0.1, 0.2
  ),
  dim = c(
    nSim(ExampleOM),
    nAge(ExampleStock),
    length(Years(ExampleOM))
  )
)

Length_CV_uniform_Age_Time <- Length(CVatAge = CVatAge) |>
  Populate(
    Ages = Ages(ExampleStock),
    Years = Years(ExampleStock),
    nSim = nSim(ExampleOM)
  )

Length_CV_uniform_Age_Time |>
  CVatAge() |>
  dimnames()


## ---- length-models ----
LengthModels()


## ---- length-vonBert_1 ----

Length_Constant <- Length(Pars = list(
  Linf = 121,
  K = 0.16,
  t0 = -1.86
)) |>
  Populate(
    Ages = Ages(ExampleStock),
    Years = Years(ExampleStock),
    nSim = nSim(ExampleOM)
  )

Length_Constant |> Pars()
Length_Constant |> MeanAtAge()

## ---- length-vonBert_2 ----
# Uniform distribution for Linf, constant for others
Length_Sim <- Length(Pars = list(
  Linf = c(121, 135),
  K = 0.16,
  t0 = -1.86
)) |>
  Populate(
    Ages = Ages(ExampleStock),
    Years = Years(ExampleStock),
    nSim = nSim(ExampleOM)
  )

Length_Sim |> Pars()
Length_Sim |> MeanAtAge()

## ---- length-vonBert_3 ----

set.seed(101)
# Historical Years
Years(ExampleStock, "H")

# Stochastic Linf values for all simulations for 2006 - 2018
Linf_06_18 <- runif(nSim(ExampleOM), 121, 135)

# Linf doubles from 2019 on
Linf_19_on <- Linf_06_18 * 2

# Create an array with named dimensions:
# Dimensions must be named `Sim` and `Year`
Linf <- array(c(Linf_06_18, Linf_19_on),
  dim = c(nSim(ExampleOM), 2),
  dimnames = list(
    Sim = 1:nSim(ExampleOM),
    Year = c(2006, 2019)
  )
)

Length_Sim_Year <- Length(Pars = list(
  Linf = Linf,
  K = 0.16,
  t0 = -1.86
)) |>
  Populate(
    Ages = Ages(ExampleStock),
    Years = Years(ExampleStock),
    nSim = nSim(ExampleOM)
  )

Length_Sim_Year |> Pars()
Length_Sim_Year |>
  MeanAtAge() |>
  MSEtool:::ArraySubsetAge(1:4)


# TODO Subset function ...


## ---- length-stock ----
Length(ExampleStock) <- Length(
  Pars = list(
    Linf = c(121, 135),
    K = c(0.16, 0.22),
    t0 = c(-1.86, -1.41)
  ),
  CVatAge = c(0.1, 0.15)
)


## ---- create-length_2 ----
# DefaultLength <- Length()
#
# Populate(DefaultLength)
#
# ?Populate
#
# Length(ExampleStock) <- Length()
#
# Populate(Length(ExampleStock))
#
# Pars=list(Linf=c(121, 135),
#           K=c(0.16, 0.22),
#           t0=c(-1.86, -1.41))
