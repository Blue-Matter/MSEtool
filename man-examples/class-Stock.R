# ---- Minimal stock (required sub-objects only) ----

## Sub-objects with empty Pars are created automatically.
## The model will not run until each sub-object is fully specified.
## Nominally Gadus morhua but with made-up paramater values!!
stk <- Stock(Name = "Atlantic cod", CommonName = "Atlantic cod",
             Species = "Gadus morhua")
stk

# ---- Fully specified stock ----

stk <- Stock(
  Name             = "Atlantic cod",
  CommonName       = "Atlantic cod",
  Species          = "Gadus morhua",
  Ages             = Ages(MaxAge = 20),
  Length           = Length(Pars = list(Linf = 120, K = 0.2, t0 = -0.1)),
  Weight           = Weight(Pars = list(a = 0.008, b = 3.0)),
  NaturalMortality = NaturalMortality(Pars = list(M = 0.2)),
  Maturity         = Maturity(Pars = list(L50 = 50, L50_95 = 10)),
  SRR              = SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4),
  Depletion        = Depletion(Final = 0.4)
)
stk

# ---- Accessing sub-objects ----

Ages(stk)
Length(stk)
NaturalMortality(stk)
SRR(stk)
Depletion(stk)

# ---- Replacing sub-objects ----

Length(stk) <- Length(Pars = list(Linf = c(100, 140), K = 0.2, t0 = -0.1))
Maturity(stk) <- Maturity(Pars = list(A50 = 4, A50_95 = 2))

# ---- Species name accessors ----

CommonName(stk)
Species(stk)
CommonName(stk) <- "Cod"
Species(stk)    <- "Gadus morhua"

# ---- Seasonal model ----

## Seasons = 4 creates quarterly time steps within each calendar year.
stk_seasonal <- Stock(
  Name    = "Seasonal stock",
  Ages    = Ages(MaxAge = 40, Units = "quarter"),
  Seasons = 4
)

Years(stk_seasonal, 'H')

# ---- Multi-stock pass-through access ----

## When Name is an OM or other S4 object with a Stock slot, Stock() is an
## accessor. For a list-valued Stock slot, pass an integer via CommonName.
## om <- OM(Stock = list(stk1, stk2))
## Stock(om)      # returns om@Stock (the list)
## Stock(om, 1)   # returns om@Stock[[1]]

# ---- Attaching to an OM ----

## om <- OM()
## Stock(om) <- stk
## Stock(om)