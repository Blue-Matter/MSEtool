#' Initialize dynamic population state in Hist
#'
#' Internal helper that constructs the initial age–area population structure
#' in a [Hist()] object by applying recruitment deviations to the equilibrium
#' unfished numbers-at-age (which already carry the spatial distribution).
#' When `Depletion@Misc$InitialAtAge` is supplied it is applied to the initial
#' numbers-at-age, which are then scaled to `Depletion@Initial`. Otherwise
#' `Depletion@Initial` is applied later by [.CalcInitialFishedEquilibrium()].
#'
#' @param Hist A [Hist()] object with initialized unfished equilibrium state.
#' @param Unfished Logical. If `TRUE`, only the recruitment deviations are
#'   applied and the initial depletion is skipped. Used for the dynamic
#'   unfished conditions.
#'
#' @return The modified [Hist()] object.
#'
#' @keywords internal
.CalcDynamicInitial <- function(Hist, Unfished = FALSE) {
  nSim <- nSim(Hist)
  
  for (st in 1:nStock(Hist)) {

    ## ---- Calculate dynamic age structure (multiply by rec devs) ----
    # Sim x Age x Area (year dim dropped; area distribution already applied)
    EquilNumber <- abind::adrop(Hist@Unfished@Equilibrium@Number[[st]][,,1,, drop=FALSE], 3)

    RecDevInit <- Hist@OM@Stock[[st]]@SRR@RecDevInit |> ExtendSims(nSim)
    RecDevHist <- Hist@OM@Stock[[st]]@SRR@RecDevHist
    
    InitYearCal <- Hist@OM@Stock[[st]]@Misc$InitYear 
    HistYears <- Years(Hist,'H')
    InitYear <- .InitYearIndex(Hist, st)
    
    RecDevHist1 <- RecDevHist[,InitYear, drop=FALSE] |> ExtendSims(nSim)
    
    names(dimnames(RecDevHist1))[2] <- 'Age'

    ages <- as.numeric(dimnames(RecDevInit)[['Age']])
    AgeClasses <- Hist@OM@Stock[[st]]@Ages@Classes
    if (min(ages) !=AgeClasses[2]) {
      cli::cli_abort(c("Error calculating initial age structure for Stock: {.val {names(Hist@OM@Stock)[st]}}",
                       "i"='The first age class in matrix `Stock |> SRR() |> RecDevInit()` must match the second age class',
                       '*'='Second age class: {.val {AgeClasses[2]}}',
                       '*'='First age class in `RecDevInit`: {.val {min(ages)}}'
      ), call=NULL)
    }

    InitialAtAge <- Hist@OM@Stock[[st]]@Depletion@Misc$InitialAtAge
    if (!Unfished && length(InitialAtAge))
      RecDevInit <- ArrayMultiply(RecDevInit, ExtendSims(InitialAtAge, nSim))

    InitAgeClassRecDevs <- cbind(RecDevHist1, RecDevInit)
    dimnames(InitAgeClassRecDevs) <- list(Sim=1:nSim,
                                          Age=c(AgeClasses[1], ages))


    ## ---- Distribute across areas ----
    # EquilNumber is Sim x Age x Area (area distribution already included in
    # Equilibrium@Number). Rec devs broadcast across areas via ArrayMultiply.
    # See `CombineOMs` for cases where InitYear > 1.
    Hist@Number[[st]][,,InitYear,] <- ArrayMultiply(
      AddDimension(InitAgeClassRecDevs, 'Area'),
      EquilNumber)

    if (InitYear>1) {
      # backfill with unfished
      eq_unfished <- EquilNumber |> ExtendSims(nSim) |>
        AddDimension('Year', val=min(HistYears), pos=3) |>
        ExtendYears(Years=HistYears[HistYears<InitYearCal])

      ArrayFill(Hist@Number[[st]]) <- eq_unfished
    }

    ## ---- Fill recruitment for initial time steps if age rec > 0  ----
    RecruitTimeStep <- .CalcRecruitmentAgeIndex(Hist, st)

    if (RecruitTimeStep>1) {
      for (ts_ind in seq_len(RecruitTimeStep - 1) + 1) {
        ts <- ts_ind + InitYear - 1
        UnfishedDist <- Hist@OM@Stock[[st]]@Spatial@UnfishedDist[,,1,ts,drop=FALSE] |>
          .Aperm(c('Sim', 'Age', 'Year', 'Area'))
      
        Recruit <- ArrayMultiply(Hist@OM@Stock[[st]]@SRR@R0[, ts, drop=FALSE],
                                 RecDevHist[, ts, drop=FALSE]) |> 
          ExtendSims(nSim) |>
          AddDimension('Area') |>
          AddDimension('Age', pos =2) 
          
        Hist@Number[[st]][,1,ts,] <- ArrayMultiply(Recruit, UnfishedDist)
      }
    }

    if (!Unfished && length(InitialAtAge) && length(Hist@OM@Stock[[st]]@Depletion@Initial))
      Hist <- .ScaleInitialDepletion(Hist, st)
    
  }

  Hist
}

# Time-step index of the first modelled time step for stock `st` (> 1 for `CombineOMs`)
.InitYearIndex <- function(Hist, st) {
  InitYearCal <- Hist@OM@Stock[[st]]@Misc$InitYear
  if (is.null(InitYearCal))
    return(1L)
  match(InitYearCal, Years(Hist, 'H'))
}

#' Initial depletion of a numbers-at-age array
#'
#' Biomass (`Reference = "B0"`) or spawning biomass (`"SB0"`) of `NAtAge`
#' relative to the unfished equilibrium in the same time step. Spawning biomass
#' uses the numbers surviving natural mortality to the time of spawning.
#'
#' @param Hist A [hist-class] object.
#' @param st Integer stock index.
#' @param NAtAge Numbers-at-age, `Sim x Age x Area`.
#' @param InitYear Time-step index.
#'
#' @return Numeric vector of length `nSim`.
#'
#' @keywords internal
.InitialDepletionRatio <- function(Hist, st, NAtAge, InitYear) {
  nSim  <- nSim(Hist)
  Stock <- Hist@OM@Stock[[st]]
  Year  <- Years(Hist, 'H')[InitYear]

  AtYear <- \(x) {
    x <- .ArraySubsetYear(x, Year)
    abind::adrop(x[, , 1, drop = FALSE], 3) |> ExtendSims(nSim)
  }

  N <- apply(NAtAge, c(1, 2), sum)
  W <- AtYear(Stock@Weight@MeanAtAge)

  Reference <- Stock@Depletion@Reference
  if (Reference == 'B0') {
    Val <- rowSums(N * W)
    Ref <- Hist@Unfished@Equilibrium@Biomass
  } else if (Reference == 'SB0') {
    M   <- AtYear(Stock@NaturalMortality@MeanAtAge)
    Mat <- AtYear(Stock@Maturity@MeanAtAge)
    STF <- rep_len(Stock@SRR@SpawnTimeFrac, nSim)
    Val <- rowSums(N * exp(-M * STF) * W * Mat)
    Ref <- Hist@Unfished@Equilibrium@SBiomass
  } else {
    cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")
  }

  Ref <- .ArraySubsetYear(Ref, Year) |> ExtendSims(nSim)
  Val / as.numeric(Ref[, st, 1])
}

#' Scale initial numbers-at-age to `Depletion@Initial`
#'
#' Scales all ages of the initial numbers-at-age by a common factor so that the
#' initial depletion equals `Depletion@Initial`. Used when the initial age
#' structure is supplied by `Depletion@Misc$InitialAtAge`.
#'
#' @param Hist A [hist-class] object.
#' @param st Integer stock index.
#'
#' @return The modified [hist-class] object.
#'
#' @keywords internal
.ScaleInitialDepletion <- function(Hist, st) {
  InitYear <- .InitYearIndex(Hist, st)
  Target   <- rep_len(Hist@OM@Stock[[st]]@Depletion@Initial, nSim(Hist))
  NAtAge   <- abind::adrop(Hist@Number[[st]][, , InitYear, , drop = FALSE], 3)
  Scale    <- Target / .InitialDepletionRatio(Hist, st, NAtAge, InitYear)
  Scale[Target == 1] <- 1
  Hist@Number[[st]][, , InitYear, ] <- NAtAge * Scale
  Hist
}

# Stocks starting from a fished equilibrium: `Depletion@Initial` < 1 with no `InitialAtAge`
.FishedInitStocks <- function(Hist) {
  purrr::map_lgl(Hist@OM@Stock, \(Stock) {
    Initial <- Stock@Depletion@Initial
    length(Initial) > 0 && any(Initial < 1) && !length(Stock@Depletion@Misc$InitialAtAge)
  })
}

#' Fished equilibrium initial conditions
#'
#' For stocks with `Depletion@Initial` < 1 and no `Depletion@Misc$InitialAtAge`,
#' replaces the initial numbers-at-age with the equilibrium reached under
#' constant fishing, scaled so that the initial depletion equals
#' `Depletion@Initial`.
#'
#' @param Hist A [hist-class] object, after [.CalcDynamicInitial()] and
#'   `.PrepHistMisc()`.
#' @param silent Logical. Suppress messages?
#'
#' @details
#' The equilibrium is found by repeating the first year's time steps through
#' the historical population dynamics until the age structure, including the
#' plus group and the spatial distribution, no longer changes. Recruitment
#' follows the stock-recruit relationship without deviations. Fishing uses the
#' biology, selectivity, retention, discard mortality, fleet `Effort x
#' Efficiency`, and seasonal effort pattern of the first historical year (or
#' `Misc$InitYear`). When no fleet has effort in that year, the effort and
#' efficiency of the first year with effort are used. A common multiplier on
#' this fishing pattern, stored in `Depletion@Misc$InitialEffortScale`, is
#' solved for each simulation and stock so that the initial biomass (or
#' spawning biomass) after applying `SRR@RecDevInit` and the first
#' `SRR@RecDevHist` equals `Depletion@Initial` times the unfished equilibrium
#' in the same time step.
#'
#' Stocks without `Depletion@Initial` are not fished while the equilibrium is
#' calculated and keep their unfished initial conditions.
#'
#' @return The modified [hist-class] object.
#'
#' @keywords internal
.CalcInitialFishedEquilibrium <- function(Hist, silent = FALSE) {
  Solve <- .FishedInitStocks(Hist)
  if (!any(Solve))
    return(Hist)

  .MsgStep("Calculating fished equilibrium initial conditions",
           "Calculated fished equilibrium initial conditions", silent)

  InitYears <- purrr::map_int(seq_len(nStock(Hist)), \(st) .InitYearIndex(Hist, st))
  for (i0 in unique(InitYears[Solve]))
    Hist <- .SolveFishedInitial(Hist, which(Solve & InitYears == i0), i0)
  Hist
}

.SolveFishedInitial <- function(Hist, Stocks, i0, tol = 1e-9, maxIter = 100) {
  nSim      <- nSim(Hist)
  nStock    <- nStock(Hist)
  nFleet    <- nFleet(Hist)
  nArea     <- nArea(Hist)
  nSeason   <- Hist@OM@Seasons
  HistYears <- Years(Hist, 'H')
  CalYears  <- floor(HistYears)
  nSolve    <- length(Stocks)
  StockNm   <- StockNames(Hist)

  Lag    <- as.integer(Hist@Misc$RecLag)
  nCarry <- pmax(Lag, 1L)
  Cyc    <- i0 + seq_len(nSeason) - 1L
  Next   <- i0 + nSeason

  if (Next + max(nCarry) - 1L > length(HistYears))
    cli::cli_abort(c("Cannot calculate fished equilibrium initial conditions",
                     "x" = "The historical period must be longer than one year plus the recruitment lag"),
                   call = NULL)

  Target <- vapply(Stocks, \(st) rep_len(as.numeric(Hist@OM@Stock[[st]]@Depletion@Initial), nSim),
                   numeric(nSim)) |> matrix(nSim, nSolve)
  Active <- Target < 1

  ## ---- fishing pattern ----
  q      <- ExtendSims(Hist@Misc$Catchability, nSim)
  Effort <- ExtendSims(Hist@Effort, nSim)

  qE <- \(sim, st, pos) sum(as.vector(q[sim, st, pos, , drop = FALSE]) *
                              as.vector(Effort[sim, pos, , drop = FALSE]))
  FallbackYear <- rep(NA_real_, nSim)
  for (sim in which(rowSums(Active) > 0)) {
    if (sum(vapply(Stocks, \(st) qE(sim, st, Cyc), numeric(1))) > 0) next
    for (cy in unique(CalYears[CalYears > CalYears[i0]])) {
      Pos <- which(CalYears == cy)
      if (length(Pos) == nSeason && sum(vapply(Stocks, \(st) qE(sim, st, Pos), numeric(1))) > 0) {
        Effort[sim, Cyc, ] <- Effort[sim, Pos, ]
        q[sim, , Cyc, ]    <- q[sim, , Pos, ]
        FallbackYear[sim]  <- cy
        break
      }
    }
    if (is.na(FallbackYear[sim]))
      cli::cli_abort(c("Cannot calculate fished equilibrium initial conditions",
                       "x" = "No historical fishing effort for Sim {sim} of {.val {StockNm[Stocks]}}"),
                     call = NULL)
  }
  if (any(!is.na(FallbackYear))) {
    Sims <- which(!is.na(FallbackYear))
    Hist <- .CaptureLog(Hist,
      string = cli::format_inline("No fishing effort in the first historical year for {cli::qty(length(Sims))}Sim{?s} {Sims}: the fished equilibrium initial conditions use the fishing pattern from {.val {unique(FallbackYear[Sims])}}"),
      name = 'Depletion', type = 'assumption')
  }

  qBase <- q
  qBase[, setdiff(seq_len(nStock), Stocks), Cyc, ] <- 0

  ## ---- scratch Hist for the spin-up ----
  Scratch <- Hist
  Scratch@Effort <- Effort
  Scratch@Misc$RecDevs <- lapply(Hist@Misc$RecDevs, \(x) { x[] <- 1; x })
  # slots written in place by CalcFisheryDynamics_(clone = 0)
  for (sl in c('SBiomass', 'SProduction'))
    methods::slot(Scratch, sl) <- methods::slot(Hist, sl) * 1
  for (sl in c('FInteractArea', 'FDeadArea', 'FRetainArea'))
    methods::slot(Scratch, sl) <- lapply(methods::slot(Hist, sl), \(x) x * 1)
  # Number is duplicated on every call, so keep only the time steps the cycle uses
  nKeep <- min(Next + max(nCarry), length(HistYears))
  Scratch@Number <- lapply(Hist@Number, \(x) x[, , seq_len(nKeep), , drop = FALSE])

  Equil <- purrr::map(seq_len(nStock), \(st) {
    .ArraySubsetYear(Hist@Unfished@Equilibrium@Number[[st]], HistYears[i0]) |>
      ExtendSims(nSim) |> abind::adrop(3)
  })
  State <- purrr::map(seq_len(nStock), \(st) list(
    Adult = Equil[[st]][, -1, , drop = FALSE],
    Age0  = Equil[[st]][, rep(1, nCarry[st]), , drop = FALSE]
  ))

  RunCycle <- function(State) {
    for (st in seq_len(nStock)) {
      N <- Scratch@Number[[st]]
      N[, , i0:nKeep, ] <- 0
      N[, -1, i0, ] <- State[[st]]$Adult
      if (Lag[st] > 0)
        N[, 1, i0 + seq_len(Lag[st]) - 1L, ] <- State[[st]]$Age0
      Scratch@Number[[st]] <- N
    }
    Res <- CalcFisheryDynamics_(Scratch, Years = HistYears[Cyc], AllYears = HistYears,
                                Sims = seq_len(nSim), nSim = nSim, nStock, nFleet, nArea,
                                DoCalcCatch = 0, DoCalcBiomass = 0, DoCalcOverallF = 0,
                                DoBackCalcEffort = 0, clone = 0)
    purrr::map(seq_len(nStock), \(st) {
      N <- Res@Number[[st]]
      Age0Pos <- if (Lag[st] > 0) Next + seq_len(Lag[st]) - 1L else i0
      list(Adult = N[, -1, Next, , drop = FALSE] |> abind::adrop(3),
           Age0  = N[, 1, Age0Pos, , drop = FALSE] |> abind::adrop(2))
    })
  }

  # state as nSim x cells matrix; Cols gives each stock's cells
  Template <- State
  Cols <- list(); n <- 0
  for (st in seq_len(nStock)) {
    p <- length(Template[[st]]$Adult) / nSim + length(Template[[st]]$Age0) / nSim
    Cols[[st]] <- n + seq_len(p)
    n <- n + p
  }
  Pack <- function(State) {
    do.call(cbind, purrr::map(State, \(x) cbind(matrix(x$Adult, nSim), matrix(x$Age0, nSim))))
  }
  Unpack <- function(X) {
    purrr::map(seq_len(nStock), \(st) {
      Y <- X[, Cols[[st]], drop = FALSE]
      nA <- length(Template[[st]]$Adult) / nSim
      list(Adult = array(Y[, seq_len(nA)], dim(Template[[st]]$Adult)),
           Age0  = array(Y[, -seq_len(nA)], dim(Template[[st]]$Age0)))
    })
  }

  # fixed-point iteration over the annual cycle with Anderson acceleration in log space
  SpinUp <- function(Lambda, State, tolN = 1e-10, maxCycle = 5000, m = 5) {
    qq <- qBase
    for (k in seq_len(nSolve))
      qq[, Stocks[k], Cyc, ] <- qBase[, Stocks[k], Cyc, , drop = FALSE] * Lambda[, k]
    Scratch@Misc$Catchability <<- qq

    X <- Pack(State)
    dF <- dG <- vector('list', nSim)
    Fprev <- Gprev <- NULL
    Collapsed <- matrix(FALSE, nSim, nSolve)
    for (i in seq_len(maxCycle)) {
      G  <- Pack(RunCycle(Unpack(X)))
      Pos <- X > 0 & G > 0
      LX <- ifelse(Pos, log(X), 0)
      LG <- ifelse(Pos, log(G), 0)
      Fr <- LG - LX

      Change <- vapply(Stocks, \(st) apply(abs(Fr[, Cols[[st]], drop = FALSE]), 1, max),
                       numeric(nSim)) |> matrix(nSim, nSolve)
      if (i %% 10 == 0)
        Collapsed <- Active & Deviation(Unpack(G)) < log(1e-4)
      if (all(Change < tolN | Collapsed)) {
        X <- G
        break
      }

      LNew <- LG
      if (!is.null(Fprev)) {
        for (s in seq_len(nSim)) {
          dF[[s]] <- cbind(dF[[s]], Fr[s, ] - Fprev[s, ])
          dG[[s]] <- cbind(dG[[s]], LG[s, ] - Gprev[s, ])
          if (ncol(dF[[s]]) > m) {
            dF[[s]] <- dF[[s]][, -1, drop = FALSE]
            dG[[s]] <- dG[[s]][, -1, drop = FALSE]
          }
          Gamma <- qr.coef(qr(dF[[s]]), Fr[s, ])
          Gamma[is.na(Gamma)] <- 0
          Step <- LG[s, ] - as.vector(dG[[s]] %*% Gamma)
          if (all(is.finite(Step)) && max(abs(Step - LG[s, ])) < 1) {
            LNew[s, ] <- Step
          } else {
            dF[s] <- dG[s] <- list(NULL)
          }
        }
      }
      Fprev <- Fr
      Gprev <- LG
      X <- ifelse(Pos, exp(LNew), G)
    }
    if (i == maxCycle)
      cli::cli_alert_warning("Fished equilibrium initial conditions not converged after {maxCycle} years")
    Unpack(X)
  }

  RecDevHist <- purrr::map(seq_len(nStock), \(st) ExtendSims(Hist@OM@Stock[[st]]@SRR@RecDevHist, nSim))
  RecDevInit <- purrr::map(seq_len(nStock), \(st) ExtendSims(Hist@OM@Stock[[st]]@SRR@RecDevInit, nSim))

  # initial numbers with deviations; with no recruitment lag, first-step recruits come from the SRR
  InitialNumbers <- function(State) {
    Init <- purrr::map(seq_len(nStock), \(st) {
      if (!st %in% Stocks)
        return(abind::adrop(Hist@Number[[st]][, , i0, , drop = FALSE], 3))
      N <- Equil[[st]] * 0
      N[, -1, ] <- State[[st]]$Adult * as.vector(RecDevInit[[st]])
      if (Lag[st] > 0)
        N[, 1, ] <- State[[st]]$Age0[, 1, ] * as.vector(RecDevHist[[st]][, i0])
      N
    })
    Lag0 <- Stocks[Lag[Stocks] == 0]
    if (length(Lag0)) {
      Sc <- Scratch
      for (st in seq_len(nStock))
        Sc@Number[[st]][, , i0, ] <- Init[[st]]
      Res <- CalcFisheryDynamics_(Sc, Years = HistYears[i0], AllYears = HistYears,
                                  Sims = seq_len(nSim), nSim = nSim, nStock, nFleet, nArea,
                                  DoCalcCatch = 0, DoCalcNumberNext = 0, DoCalcTransition = 0,
                                  DoCalcBiomass = 0, DoCalcOverallF = 0, DoBackCalcEffort = 0,
                                  clone = 0)
      for (st in Lag0)
        Init[[st]][, 1, ] <- Res@Number[[st]][, 1, i0, , drop = FALSE] * as.vector(RecDevHist[[st]][, i0])
    }
    Init
  }

  Deviation <- function(State) {
    Init <- InitialNumbers(State)
    vapply(seq_len(nSolve), \(k) {
      st <- Stocks[k]
      N <- Init[[st]]
      # spawning biomass in the first step is calculated before that step's recruitment
      if (Lag[st] == 0 && Hist@OM@Stock[[st]]@Depletion@Reference == 'SB0')
        N[, 1, ] <- 0
      log(.InitialDepletionRatio(Hist, st, N, i0) / Target[, k])
    }, numeric(nSim)) |> matrix(nSim, nSolve)
  }

  ## ---- solve effort scale (log space, bracketed Illinois) ----
  qE0 <- vapply(Stocks, \(st) vapply(seq_len(nSim), \(sim) {
    max(vapply(Cyc, \(pos) qE(sim, st, pos), numeric(1)))
  }, numeric(1)), numeric(nSim)) |> matrix(nSim, nSolve)
  x0 <- log(0.2 / nSeason / pmax(qE0, tiny))
  xMax <- x0 + log(1e4)
  xMin <- x0 - log(1e8)

  Eval <- function(x, tolN = 1e-10) {
    Lambda <- exp(x)
    Lambda[!Active] <- 0
    State <<- SpinUp(Lambda, State, tolN)
    g <- Deviation(State)
    g[!Active] <- 0
    # restart collapsed stocks from unfished; recovery from near zero is very slow
    for (k in seq_len(nSolve)) {
      Collapsed <- g[, k] < log(1e-4)
      if (!any(Collapsed)) next
      st <- Stocks[k]
      State[[st]]$Adult[Collapsed, , ] <<- Equil[[st]][Collapsed, -1, , drop = FALSE]
      State[[st]]$Age0[Collapsed, , ] <<- Equil[[st]][Collapsed, rep(1, nCarry[st]), , drop = FALSE]
    }
    g
  }

  x <- x0
  g <- Eval(x, 1e-4)
  lo <- hi <- glo <- ghi <- matrix(NA_real_, nSim, nSolve)
  Side <- matrix(0, nSim, nSolve)
  Stuck <- matrix(FALSE, nSim, nSolve)

  for (it in seq_len(maxIter)) {
    Up <- Active & g > 0
    if (any(Up & Side == 1)) ghi[Up & Side == 1] <- ghi[Up & Side == 1] / 2
    lo[Up] <- x[Up]; glo[Up] <- g[Up]
    Dn <- Active & g <= 0
    if (any(Dn & Side == -1)) glo[Dn & Side == -1] <- glo[Dn & Side == -1] / 2
    hi[Dn] <- x[Dn]; ghi[Dn] <- g[Dn]
    Side[Up] <- 1; Side[Dn] <- -1

    Stuck <- Stuck | (Active & is.na(hi) & x >= xMax) | (Active & is.na(lo) & x <= xMin)
    Todo <- Active & abs(g) > tol & !Stuck
    if (!any(Todo)) break

    Both <- !is.na(lo) & !is.na(hi)
    xNew <- x
    xNew[Todo & is.na(hi)] <- x[Todo & is.na(hi)] + log(4)
    xNew[Todo & is.na(lo)] <- x[Todo & is.na(lo)] - log(4)
    FP <- Todo & Both
    xNew[FP] <- lo[FP] - glo[FP] * (hi[FP] - lo[FP]) / (ghi[FP] - glo[FP])
    # bisect when an end is far from the target (e.g. collapsed), where false position stalls
    Bisect <- FP & pmax(abs(glo), abs(ghi)) > 1
    xNew[Bisect] <- (lo[Bisect] + hi[Bisect]) / 2
    x <- xNew
    # inner solve only needs to be accurate relative to the current outer residual
    g <- Eval(x, max(1e-10, min(1e-4, 1e-5 * max(abs(g[Todo])))))
  }

  if (any(Stuck)) {
    Bad <- which(Stuck, arr.ind = TRUE)
    cli::cli_alert_warning("{.field Depletion@Initial} could not be reached for {.val {unique(StockNm[Stocks[Bad[, 2]]])}} in {cli::qty(length(unique(Bad[, 1])))}Sim{?s} {unique(Bad[, 1])}")
  } else if (any(Active & abs(g) > 1e-6)) {
    cli::cli_alert_warning("Fished equilibrium initial conditions did not fully converge to {.field Depletion@Initial}")
  }

  ## ---- write initial conditions ----
  Init <- InitialNumbers(State)
  for (k in seq_len(nSolve)) {
    st  <- Stocks[k]
    Act <- Active[, k]
    N0  <- Init[[st]]
    if (Lag[st] == 0)
      N0[, 1, ] <- 0
    Hist@Number[[st]][Act, , i0, ] <- N0[Act, , , drop = FALSE]
    if (Lag[st] > 1) {
      for (p in seq_len(Lag[st] - 1L))
        Hist@Number[[st]][Act, 1, i0 + p, ] <- State[[st]]$Age0[Act, p + 1L, , drop = FALSE] *
          RecDevHist[[st]][Act, i0 + p]
    }
    Scale <- exp(x[, k])
    Scale[!Act] <- 0
    Hist@OM@Stock[[st]]@Depletion@Misc$InitialEffortScale <- array(Scale, nSim, list(Sim = seq_len(nSim)))
  }
  Hist
}

#' Calculate Recruitment Age Index for One or All Stocks
#'
#' Returns the recruitment lag (in time steps) for each stock in an `om` or
#' `hist` class object: `SRR@SpawnLag` when set, otherwise the number of time
#' steps from spawning to the youngest age class. The first `lag` time steps of
#' age-0 numbers are filled from the initial conditions.
#'
#' @param OM  An `om` or `hist` class object.
#' @param st  Integer or `NULL`. If provided, returns the recruitment lag
#'            for stock `st` only. Default: `NULL` (all stocks).
#'
#' @return If `st` is provided, a single integer. Otherwise, an array of
#'   recruitment lags with a `Stock` dimension.
#'
#' @keywords internal
.CalcRecruitmentAgeIndex <- function(OM, st=NULL) {
  .CheckClass(OM, c('om', 'hist'))

  if (inherits(OM, 'hist'))
    OM <- OM@OM

  if (!is.null(st))
    return(unname(.RecLag(OM@Stock[st])))

  .RecLag(OM@Stock) |>
    as.list() |>
    List2Array('Stock') |>
    DropDimension('Sim')
}
