library(MSEtool)

## ---- seasonalspatial-create-spatial-arrays ----
AgesObj  <- Ages(MaxAge = 36, Units = 'month')
nAge_ss  <- nAge(AgesObj)
nArea_ss <- 3
area_ss  <- seq_len(nArea_ss)

peak_month_ss <- seq(1, nAge_ss, length.out = nArea_ss)
sigma_ss      <- nAge_ss / (nArea_ss * 1.5)

UnfishedDist_ss <- array(
  0,
  dim      = c(1, nArea_ss, nAge_ss),
  dimnames = list(Sim  = 1,
                  Area = area_ss,
                  Age  = Classes(AgesObj))
)
for (a in area_ss) {
  UnfishedDist_ss[1, a, ] <- exp(-0.5 * (
    (seq_len(nAge_ss) - peak_month_ss[a]) 
    / sigma_ss
  )^2
  )
}

UnfishedDist_ss <- sweep(UnfishedDist_ss, 
                         3, 
                         apply(UnfishedDist_ss, 3, sum), "/")

FracOther_ss <- array(
  NA,
  dim      = c(1, nArea_ss, nArea_ss),
  dimnames = list(Sim = 1, FromArea = area_ss, ToArea = area_ss)
)
FracOther_ss[1, 1, ] <- c(NA,   1,    0.02)
FracOther_ss[1, 2, ] <- c(1,    NA,   1   )
FracOther_ss[1, 3, ] <- c(0.02, 1,    NA  )

## ---- seasonalspatial-create-stock ----
SeasonalSpatialExStock <- Stock(
  Name       = "Example Seasonal Spatial Stock",
  CommonName = "Reef Fish",
  Species    = "Example species",

  Ages = AgesObj,

  Length = Length(
    Pars    = list(Linf = 30, K = 0.8, t0 = -0.1),
    CVatAge = 0.1
  ),

  Weight = Weight(
    Pars = list(alpha = 1e-5, beta = 3)
  ),

  NaturalMortality = NaturalMortality(
    Pars = list(M = 0.09)
  ),

  Maturity = Maturity(
    Pars = list(L50 = 15, L50_95 = 3)
  ),

  SRR = SRR(
    Pars = list(h = 0.7),
    R0   = SetSeasonalR0(AnnualR0   = 1000,
                         Seasons    = 12,
                         PeakSeason = 6,
                         Sigma      = 1.5),
    SD   = 0.4,
    AC   = 0.3
  ),

  Spatial = Spatial(
    UnfishedDist = UnfishedDist_ss,
    ProbStaying  = c(0.85, 0.80, 0.90),
    FracOther    = FracOther_ss,
    RelativeSize = c(0.15, 0.35, 0.50)
  ),

  Depletion = Depletion(Final = 0.35, Reference = "B0")
)

## ---- save object ----
usethis::use_data(SeasonalSpatialExStock, overwrite = TRUE)
