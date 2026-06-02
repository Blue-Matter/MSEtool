OM2stock <- function(OM, 
                     cpars = NULL, 
                     YearsList = NULL, 
                     nSim, 
                     seed = NULL) {
  
  if (inherits(OM, 'OM')) 
    cpars <- OM@cpars
  
  CurrentYear <- YearsList$CurrentYear
  nYear       <- YearsList$nYear
  pYear       <- YearsList$pYear
  Seasons     <- YearsList$Seasons
  
  stock                  <- Stock2Name(OM)
  stock@Ages             <- Stock2Ages(OM, Seasons = Seasons)
  stock@Years            <- c(YearsList$HistTS, YearsList$ProjTS)
  stock@Seasons          <- YearsList$Seasons
  stock@Length           <- OM2Length(OM, cpars = cpars, Years= stock@Years, 
                                      nSim = nSim, Seasons = Seasons)
  stock@Weight           <- OM2Weight(OM, cpars = cpars, Years=stock@Years)
  stock@NaturalMortality <- OM2NaturalMortality(OM, cpars, Years=stock@Years,
                                                nSim = nSim, Seasons = Seasons)
  stock@Maturity         <- OM2Maturity(OM, cpars, Years=stock@Years, 
                                        nSim = nSim, Seasons = Seasons)
  stock@Fecundity        <- OM2Fecundity(OM, cpars, Years=stock@Years, 
                                         nSim = nSim, Seasons = Seasons)
  stock@SRR              <- OM2SRR(OM, cpars, YearsList=YearsList, 
                                   nSim = nSim, Seasons = Seasons)
  stock@Spatial          <- OM2Spatial(OM, cpars)
  stock@Depletion        <- OM2Depletion(OM, cpars)
  
  PopulateStock(Stock = stock, 
                nYear = nYear,
                pYear = pYear,
                CurrentYear = CurrentYear,
                nSim = nSim,
                Seasons = YearsList$Seasons,
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

GetStockAges <- function(Stock, Seasons = 1) {
  CheckClass(Stock, "Stock", "Stock")
  (0:Stock@maxage)/Seasons
}

OM2Length <- function(OM, cpars = list(), Years=NULL, nSim=NULL, Seasons = 1) {
  Length <- Stock2Length(OM)
  
  if (!length(cpars)) 
    return(Length)
  
  if (is.null(Years)) 
    Years <- GetOMYears(OM)
  
  if (is.null(nSim))
    nSim <- OM@nsim

  # MeanAtAge
  if (!is.null(cpars$Len_age)) {
    Length <- SetSlotDimNames(Length, 'MeanAtAge', cpars$Len_age,
                              Sims  = seq_len(nSim),
                              Ages  = GetStockAges(OM, Seasons),
                              Years = Years)
  }
  
  # CVatAge
  if (!is.null(cpars$LenCV)) {
    val <- array(cpars$LenCV, dim = c(length(cpars$LenCV), 1, 1))
    Length <- SetSlotDimNames(Length, 'CVatAge', val,
                              Sims  = seq_len(nSim),
                              Ages  = GetStockAges(OM, Seasons),
                              Years = Years)
  }

  # Classes
  if (!is.null(OM@cpars$CAL_binsmid))
    Length@Classes <- OM@cpars$CAL_binsmid


  # Pars
  if (!is.null(cpars$Linf)) 
    Length@Pars$Linf <- cpars$Linf
  
  if (!is.null(cpars$K)) 
    Length@Pars$K <- cpars$K
  
  if (!is.null(cpars$t0)) 
    Length@Pars$t0 <- cpars$t0
  
  if (!is.null(Length@MeanAtAge)) 
    Length@Pars <- list()
    Length@Model <- NULL
  
  Length
}

OM2Weight <- function(OM, cpars = list(), Years=NULL) {
  Weight <- Stock2Weight(OM)
  
  if (!length(cpars)) 
    return(Weight)
  
  if (!is.null(cpars[["a"]])) 
    Weight@Pars$alpha <- cpars[["a"]]

  if (!is.null(cpars[["b"]])) 
    Weight@Pars$beta <- cpars[["b"]]
  
  if (!is.null(Weight@MeanAtAge)) {
    Weight@Pars <- list()
    Weight@Model <- NULL
  }
  
  Weight
}

OM2NaturalMortality <- function(OM, cpars = list(), Years=NULL, nSim=NULL, Seasons = 1) {
  NaturalMortality <- Stock2NaturalMortality(OM)
  if (!length(cpars)) 
    return(NaturalMortality)
  

  if (is.null(Years))
    Years <- GetOMYears(OM)
  
  if (is.null(nSim))
    nSim <- OM@nsim
  
  if (!is.null(cpars$M_ageArray)) {
    NaturalMortality <- SetSlotDimNames(NaturalMortality, 'MeanAtAge', cpars$M_ageArray,
                              Sims  = seq_len(nSim),
                              Ages  = GetStockAges(OM, Seasons),
                              Years = Years)
  }
  if (!is.null(NaturalMortality@MeanAtAge)) {
    NaturalMortality@Pars <- list()
    NaturalMortality@Model <- NULL
  }
  
  NaturalMortality
}

OM2Maturity <- function(OM, cpars = list(), Years=NULL, nSim=NULL, Seasons = 1) {
  Maturity <- Stock2Maturity(OM)
  
  if (!length(cpars)) 
    return(Maturity)

  if (is.null(Years))
    Years <- GetOMYears(OM)
  
  if (is.null(nSim))
    nSim <- OM@nsim
  
  if (!is.null(cpars$Mat_age)) {
    Maturity <- SetSlotDimNames(Maturity, 'MeanAtAge', cpars$Mat_age,
                                        Sims  = seq_len(nSim),
                                        Ages  = GetStockAges(OM, Seasons),
                                        Years = Years)
  }
  if (!is.null(Maturity@MeanAtAge)) {
    Maturity@Pars <- list()
    Maturity@Model <- NULL
  }
  
  Maturity
}

OM2Fecundity <- function(OM, cpars = list(), Years=NULL, nSim=NULL, Seasons = 1) {
  Fecundity <- Stock2Fecundity(OM)
  
  if (is.null(Years))
    Years <- GetOMYears(OM)
  
  if (is.null(nSim))
    nSim <- OM@nsim
  
  if (!is.null(cpars$Fec_age)) {
    Fecundity <- SetSlotDimNames(Fecundity, 'MeanAtAge', cpars$Fec_age,
                                Sims  = seq_len(nSim),
                                Ages  = GetStockAges(OM, Seasons),
                                Years = Years)
  }
  
  if (!is.null(Fecundity@MeanAtAge)) {
    Fecundity@Pars <- list()
    Fecundity@Model <- NULL
  }
  
  Fecundity
}

OM2SRR <- function(OM, cpars = list(), YearsList=NULL, nSim=NULL, Seasons = 1) {
  SRR <- Stock2SRR(OM)
  if (!length(cpars)) 
    return(SRR)
  
  if (is.null(nSim))
    nSim <- OM@nsim

  if (!is.null(cpars$hs)) {
    if (SRR@Model == 'Ricker') {
      SRR@Pars$hR <- cpars$hs
    } else {
      SRR@Pars$h <- cpars$hs  
    }
  }

  if (is.null(YearsList)) {
    Years <- GetOMYears(OM)
    HistYears <- GetOMYears(OM, "H")
    ProjYears <- GetOMYears(OM, "P")
    
  } else {
    Years <- c(YearsList$HistTS, YearsList$ProjTS)
    HistYears <- YearsList$HistTS
    ProjYears <- YearsList$ProjTS
  }
  
  if (!is.null(cpars$R0)) {
    SRR@R0 <- array(cpars$R0,
      dim = c(length(cpars$R0), 1),
      dimnames = list(
        Sim = 1:nSim,
        Year = Years[1]
      )
    ) |> ReduceDims()
  }

  if (!is.null(cpars$AC)) {
    SRR@AC <- array(cpars$AC,
      dim = c(length(cpars$R0), 1),
      dimnames = list(
        Sim = 1:nSim,
        Year = Years[1]
      )
    ) |> ReduceDims()
  }

  perr_y <- cpars[["Perr_y"]]

  if (!is.null(perr_y)) {

    nYear <- length(HistYears)
 
    proyears <- length(ProjYears)
    AgesClasses <- GetStockAges(OM, Seasons)
    n_age <- length(AgesClasses)
    
    init_age_classes <- perr_y[, (n_age-1):1]
    SRR@RecDevInit <- array(init_age_classes,
      dim = dim(init_age_classes),
      dimnames = list(
        Sim = 1:nrow(init_age_classes),
        Age = AgesClasses[-1]
      )
    ) |> ReduceDims()

    hist_yrs <- perr_y[, (n_age):(nYear + n_age-1)]
    SRR@RecDevHist <- array(hist_yrs,
      dim = dim(hist_yrs),
      dimnames = list(
        Sim = 1:nrow(hist_yrs),
        Year = HistYears
      )
    ) |> ReduceDims()

    pro_yrs <- perr_y[, (nYear + n_age):(nYear + n_age - 1 + proyears)]
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
    cli::cli_alert_warning("Importing Movement matrix from cpars currently not supported")
    # Spatial@Movement <- process_mov(cpars$mov)  
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

  mov <- aperm(mov, c(1, 4, 5, 3, 2))
  # dd <- dim(mov)
  # dimnames(mov) <- list(
  #   Sim = 1:dd[1],
  #   FromArea = 1:dd[2],
  #   ToArea =  1:dd[3],
  #   Year = NULL # years missing from function
  # )
      

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
    Depletion@Final <- NULL # don't use Stock2Depletion if D exists in cpars
    if (is.null(cpars[["qs"]])) {
      # only use it if qs hasn't been set
      Depletion@Final <- cpars[["D"]]
    }
  }
  Depletion
}



