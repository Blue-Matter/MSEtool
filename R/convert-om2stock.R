OM2stock <- function(OM, 
                     cpars = NULL, 
                     YearsList = NULL, 
                     nSim, 
                     seed = NULL) {
  
  if (inherits(OM, 'OM')) {
    cpars <- OM@cpars
  }
  
  CurrentYear <- floor(YearsList$HistTS) |> max()
  nYear <- length(YearsList$HistTS)
  pYear <- length(YearsList$ProjTS)
  
  stock <- Stock2Name(OM)
  stock@Ages <- Stock2Ages(OM)
  stock@Years <- c(YearsList$HistTS, YearsList$ProjTS)
  stock@Seasons <- YearsList$Seasons
  stock@Length <- OM2Length(OM, cpars = cpars)
  stock@Weight <- OM2Weight(OM, cpars = cpars)
  stock@NaturalMortality <- OM2NaturalMortality(OM, cpars)
  stock@Maturity <- OM2Maturity(OM, cpars)
  stock@Fecundity <- OM2Fecundity(OM, cpars)
  stock@SRR <- OM2SRR(OM, cpars)
  stock@Spatial <- OM2Spatial(OM, cpars)
  stock@Depletion <- OM2Depletion(OM, cpars)
  
  PopulateStock(Stock = stock, 
                nYear = nYear,
                pYear = pYear,
                CurrentYear = CurrentYear,
                nSim = nSim,
                seed = seed, 
                silent = TRUE)
}

GetOMYears <- function(OM, Period = NULL) {
  CheckClass(OM, c("OM", "MOM"))
  HistYears <- seq(OM@CurrentYr, by = -1, length.out = OM@nyears) |> rev()
  ProjYears <- seq(OM@CurrentYr + 1, by = 1, length.out = OM@proyears)

  if (is.null(Period)) {
    return(c(HistYears, ProjYears))
  }
  if (grepl("H", Period)) {
    return(HistYears)
  }

  if (grepl("P", Period)) {
    return(ProjYears)
  }
  c(HistYears, ProjYears)
}

GetStockAges <- function(Stock) {
  CheckClass(Stock, "Stock", "Stock")
  0:Stock@maxage
}

OM2Length <- function(OM, cpars = list()) {
  Length <- Stock2Length(OM)
  if (!length(cpars)) {
    return(Length)
  }

  # MeanAtAge
  if (!is.null(cpars$Len_age)) {
    Length@MeanAtAge <- cpars$Len_age
    dimnames(Length@MeanAtAge) <- list(
      Sim = 1:OM@nsim,
      Age = GetStockAges(OM),
      Year = GetOMYears(OM)
    )
    Length@MeanAtAge <- ReduceDims(Length@MeanAtAge)
  }

  # CVatAge
  if (!is.null(cpars$LenCV)) {
    Length@CVatAge <- array(cpars$LenCV, dim = c(length(cpars$LenCV), 1, 1))
    dimnames(Length@CVatAge) <- list(
      Sim = 1:OM@nsim,
      Age = GetStockAges(OM)[1],
      Year = GetOMYears(OM)[1]
    )
    Length@CVatAge <- ReduceDims(Length@CVatAge)
  }

  # Classes


  # Pars
  if (!is.null(cpars$Linf)) {
    Length@Pars$Linf <- cpars$Linf
  }

  if (!is.null(cpars$K)) {
    Length@Pars$K <- cpars$K
  }

  if (!is.null(cpars$t0)) {
    Length@Pars$t0 <- cpars$t0
  }

  Length
}

OM2Weight <- function(OM, cpars = list()) {
  Weight <- Stock2Weight(OM)
  if (!length(cpars)) {
    return(Weight)
  }
  if (!is.null(cpars[["a"]])) {
    Weight@Pars$alpha <- cpars[["a"]]
  }

  if (!is.null(cpars[["b"]])) {
    Weight@Pars$beta <- cpars[["b"]]
  }
  Weight
}

OM2NaturalMortality <- function(OM, cpars = list()) {
  NaturalMortality <- Stock2NaturalMortality(OM)
  if (!length(cpars)) {
    return(NaturalMortality)
  }

  if (!is.null(cpars$M_ageArray)) {
    NaturalMortality@MeanAtAge <- array(cpars$M_ageArray,
      dim = dim(cpars$M_ageArray),
      dimnames = list(
        Sim = 1:OM@nsim,
        Age = GetStockAges(OM),
        Year = GetOMYears(OM)
      )
    ) |>
      ReduceDims()
  }
  NaturalMortality
}

OM2Maturity <- function(OM, cpars = list()) {
  Maturity <- Stock2Maturity(OM)
  if (!length(cpars)) {
    return(Maturity)
  }
  if (!is.null(cpars$Mat_age)) {
    Maturity@MeanAtAge <- array(cpars$Mat_age,
      dim = dim(cpars$Mat_age),
      dimnames = list(
        Sim = 1:OM@nsim,
        Age = GetStockAges(OM),
        Year = GetOMYears(OM)
      )
    ) |>
      ReduceDims()
  }
  Maturity
}

OM2Fecundity <- function(OM, cpars = list()) {
  Fecundity <- Stock2Fecundity(OM)
  if (!is.null(cpars$Fec_age)) {
    Fecundity@MeanAtAge <- array(cpars$Fec_age,
      dim = dim(cpars$Fec_age),
      dimnames = list(
        Sim = 1:OM@nsim,
        Age = GetStockAges(OM),
        Year = GetOMYears(OM)
      )
    ) |>
      ReduceDims()
  }
  Fecundity
}

OM2SRR <- function(OM, cpars = list()) {
  SRR <- Stock2SRR(OM)
  if (!length(cpars)) {
    return(SRR)
  }

  if (!is.null(cpars$hs)) {
    SRR@Pars$h <- cpars$hs
  }

  if (!is.null(cpars$R0)) {
    SRR@R0 <- array(cpars$R0,
      dim = c(length(cpars$R0), 1),
      dimnames = list(
        Sim = 1:OM@nsim,
        Year = GetOMYears(OM)[1]
      )
    ) |> ReduceDims()
  }

  if (!is.null(cpars$AC)) {
    SRR@AC <- array(cpars$AC,
      dim = c(length(cpars$R0), 1),
      dimnames = list(
        Sim = 1:OM@nsim,
        Year = GetOMYears(OM)[1]
      )
    ) |> ReduceDims()
  }

  perr_y <- cpars[["Perr_y"]]

  if (!is.null(perr_y)) {
    HistYears <- GetOMYears(OM, "H")
    nYear <- length(HistYears)
    ProjYears <- GetOMYears(OM, "P")
    proyears <- length(ProjYears)
    AgesClasses <- GetStockAges(OM)
    maxage <- max(AgesClasses)

    init_age_classes <- perr_y[, maxage:1]
    SRR@RecDevInit <- array(init_age_classes,
      dim = dim(init_age_classes),
      dimnames = list(
        Sim = 1:nrow(init_age_classes),
        Age = AgesClasses[-1]
      )
    ) |> ReduceDims()

    hist_yrs <- perr_y[, (maxage + 1):(nYear + maxage)]
    SRR@RecDevHist <- array(hist_yrs,
      dim = dim(hist_yrs),
      dimnames = list(
        Sim = 1:nrow(hist_yrs),
        Year = HistYears
      )
    ) |> ReduceDims()

    pro_yrs <- perr_y[, (nYear + maxage + 1):(nYear + maxage + proyears)]
    SRR@RecDevProj <- array(pro_yrs,
      dim = dim(pro_yrs),
      dimnames = list(
        Sim = 1:nrow(pro_yrs),
        Year = ProjYears
      )
    ) |> ReduceDims()
  }

  SRR
}

OM2Spatial <- function(OM, cpars = list) {
  Spatial <- Stock2Spatial(OM)
  if (!length(cpars)) {
    return(Spatial)
  }
  
  if (!is.null(cpars$Asize)) {
    Spatial@RelativeSize <- array(cpars$Asize,
                                  dim=dim(cpars$Asize),
                                  dimnames=list(
                                    Sim=1:nrow(cpars$Asize),
                                    Area=1:ncol(cpars$Asize)
                                  )
    )
  }
  
  if (!is.null(cpars$mov)) {
    Spatial@Movement <- process_mov(cpars$mov)  
  }
  
  Spatial
}

process_mov <- function(mov, nage = 1, nts = 1) {
  dd <- dim(mov)
  if (is.null(dd)) {
    return(NULL)
  }
  if (length(dd) < 3) {
    stop("`mov` must be an array with at dimensions `nsim`, `narea`, `narea`")
  }
  if (length(dd) == 3) {
    # add age and time-step
    mov <- abind::abind(mov, array(0, dim = c(0, dd)), along = 1)
    mov <- abind::abind(mov, array(0, dim = c(0, 1, dd)), along = 1)
    mov <- aperm(mov, c(3, 1, 2, 4, 5))
  }
  if (length(dd) == 4) {
    # add time-step
    mov <- abind::abind(mov, array(0, dim = c(0, dd)), along = 1)
    mov <- aperm(mov, c(2, 1, 3, 4, 5))
  }

  mov <- aperm(mov, c(1, 4, 5, 3, 2)) |>
    AddDimNames(c("Sim", "Area", "Area", "Age", "Year")) |>
    ReduceDims()

  mov
}


OM2Depletion <- function(OM, cpars = list()) {
  Depletion <- Stock2Depletion(OM)
  if (!length(cpars)) {
    return(Depletion)
  }
  if (!is.null(cpars$initD)) {
    Depletion@Initial <- cpars$initD  
  }
  
  if (!is.null(cpars[["D"]])) {
    if (is.null(cpars[["qs"]])) {
      Depletion@Final <- cpars[["D"]]
    }
  }
  Depletion
}



