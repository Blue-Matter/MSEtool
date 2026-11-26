#' Condition Composition Observation Error from Observed Data
#'
#' Internal function to condition [compobs-class] observation error parameters
#' from observed age or size composition data. Populates `Shift` (per-bin
#' log-concentration residual), `ESS` (effective sample size), and
#' `SampleSize` for each fleet or survey where observed composition data are
#' available.
#'
#' @param Hist A [Hist()] object populated with historical fishery dynamics.
#' @param FisheryData A [Data()] object containing real fishery composition
#'   data.
#' @param HistYears Numeric vector of historical years.
#' @param ProjYears Numeric vector of projection years.
#' @param stocks Integer vector of stock indices in the complex.
#' @param i Integer index of the observed data set.
#' @param type Character; one of `"LandingsAtAge"`, `"DiscardsAtAge"`,
#'   `"LandingsAtSize"`, `"DiscardsAtSize"`, `"SurveyAtAge"`, or
#'   `"SurveyAtSize"`.
#'
#' @details
#'
#' ## OM-Predicted Compositions
#'
#' True catch-at-age or catch-at-size is aggregated over the stocks of the
#' data element and over areas (see [GenDataComp]); survey compositions are
#' predicted from the survey's selected numbers. Fleets are not required to
#' share a size-class grid (see [compdata-class]). If observed and
#' OM-predicted size classes don't have matching counts, this is treated as a
#' mismatch between `FisheryData` and the operating model and raises an error
#' rather than silently comparing mismatched bins.
#'
#' Predicted proportions \eqn{\hat{p}_{s,t,b}} are computed by normalising
#' within each simulation and year.
#'
#' ## Stock-specific data
#'
#' When the composition slot of `FisheryData` is a list named by the complex
#' and/or stocks (see [data-class]), each element is conditioned against the
#' predicted composition of its stocks, and the [compobs-class] slot of the
#' `obs` object becomes a list with the same names. A fleet that is `Joint` in
#' several stock elements is conditioned as one sample over the classes of all
#' those elements: `SampleSize` and `ESS` are shared, and `Shift` is split back
#' to each element.
#'
#' @return `Hist` with `SampleSize`, `Shift`, and `ESS` populated in the
#'   relevant [compobs-class] slots of `Hist@OM@Obs[[i]]` for each fleet or
#'   survey where observed composition data are available.
#'
#' @keywords internal
.ConditionObsComp <- function(Hist,
                              FisheryData,
                              HistYears,
                              ProjYears,
                              stocks,
                              i,
                              type = c('LandingsAtAge', 'DiscardsAtAge',
                                       'LandingsAtSize', 'DiscardsAtSize',
                                       'SurveyAtAge', 'SurveyAtSize')) {

  type     <- match.arg(type)
  Complex  <- names(Hist@OM@Complexes)[i]
  DataEls  <- .SlotElements(slot(FisheryData, type), Complex)
  DataEls  <- DataEls[!purrr::map_lgl(DataEls, EmptyObject)]
  if (!length(DataEls))
    return(Hist)

  isAge    <- .CompIsAge(type)
  BinDim   <- if (isAge) 'Age' else 'Class'
  nSim     <- nSim(Hist)
  YearsAll <- c(HistYears, ProjYears)
  StockNms <- StockNames(Hist@OM)
  ElNames  <- purrr::map(DataEls, \(cd) dimnames(cd@Value)[[2]] %||% cd@Name)
  Names    <- intersect(unique(unlist(ElNames)), names(Hist@OM@Obs[[i]]))

  for (nm in Names) {
    ObsSlot <- slot(Hist@OM@Obs[[i]][[nm]], type)
    ElsNm   <- names(DataEls)[purrr::map_lgl(ElNames, \(x) nm %in% x)]
    BaseObs <- purrr::map(ElsNm, \(el) .ElementObs(ObsSlot, el) %||% new('compobs')) |>
      stats::setNames(ElsNm)
    Units   <- .CompSampleUnits(ElsNm, purrr::map_lgl(ElsNm, \(el) .IsJointData(DataEls[[el]], nm)),
                                Complex)
    Conditioned <- list()

    for (Unit in Units) {
      CompObs   <- BaseObs[[Unit[1]]]
      CondYears <- intersect(CompObs@Years %||% HistYears, HistYears)
      if (!length(CondYears)) next

      Pred <- purrr::map(Unit, \(el) {
        elStocks <- .ElementStocks(el, Complex, StockNms, stocks)
        .TrueComp(Hist, type, elStocks, nm, CondYears, seq_len(nSim))
      }) |> stats::setNames(Unit)
      if (any(purrr::map_lgl(Pred, is.null))) next
      nBin <- purrr::map_int(Pred, \(p) dim(p)[2])

      ObsCounts <- purrr::map(Unit, \(el) {
        Value   <- DataEls[[el]]@Value
        nObsBin <- length(.CompClasses(DataEls[[el]], nm))
        if (!isAge && nObsBin != nBin[[el]])
          cli::cli_abort(c(
            "{.val {nm}}: observed and OM-predicted {.field {type}} have different numbers of size classes ({nObsBin} vs {nBin[[el]]}).",
            "i" = "This indicates a mismatch between the operating model's size classes and the supplied `FisheryData`."
          ), call = NULL)
        yr <- match(as.character(CondYears), dimnames(Value)[[1]])
        matrix(Value[yr, match(nm, ElNames[[el]]), seq_len(nBin[[el]])], nrow = length(CondYears))
      })
      ObsCounts <- do.call(cbind, ObsCounts)
      PredAll   <- abind::abind(Pred, along = 2, use.dnns = TRUE)
      BinNames  <- unlist(purrr::map(Pred, \(p) dimnames(p)[[2]]), use.names = FALSE)
      dimnames(ObsCounts) <- list(Year = CondYears, Bin = BinNames)

      UnitObs <- .ConditionCompUnit(CompObs, ObsCounts, PredAll, CondYears, ProjYears, YearsAll,
                                    BinNames, BinDim, nSim)
      if (is.null(UnitObs)) next

      Split <- rep(Unit, nBin)
      for (el in Unit) {
        ObsEl <- BaseObs[[el]]
        ObsEl@SampleSize <- UnitObs@SampleSize
        ObsEl@ESS        <- UnitObs@ESS
        ObsEl@Shift      <- UnitObs@Shift[, , Split == el, drop = FALSE]
        if (length(Unit) > 1) ObsEl@Joint <- TRUE
        Conditioned[[el]] <- ObsEl
      }
    }
    if (!length(Conditioned)) next

    if (!is.list(ObsSlot) && identical(names(Conditioned), Complex) && length(DataEls) == 1) {
      ObsSlot <- Conditioned[[1]]
    } else {
      Others  <- if (is.list(ObsSlot)) ObsSlot[setdiff(names(ObsSlot), names(Conditioned))] else list()
      ObsSlot <- c(Conditioned, Others)
    }
    slot(Hist@OM@Obs[[i]][[nm]], type) <- ObsSlot
  }

  Hist
}

# SampleSize, Shift and ESS of one sample of compositions:
# ObsCounts [nCondYear x nBin], PredCatch [nSim x nBin x nCondYear]
.ConditionCompUnit <- function(CompObs, ObsCounts, PredCatch, CondYears, ProjYears, YearsAll,
                               BinNames, BinDim, nSim) {
  nProjTS    <- length(ProjYears)
  nYearsAll  <- length(YearsAll)
  nCondYears <- length(CondYears)
  nBin       <- length(BinNames)

  UserSS    <- if (.UserSupplied(CompObs, "SampleSize")) .ProjYearValues(CompObs@SampleSize, nSim, ProjYears)
  UserESS   <- if (.UserSupplied(CompObs, "ESS")) .ProjYearValues(CompObs@ESS, nSim, ProjYears)
  UserShift <- if (.UserSupplied(CompObs, "Shift")) CompObs@Shift

  ObsTotals <- rowSums(ObsCounts, na.rm = TRUE)
  valid_yrs <- which(ObsTotals > 0 & !is.na(ObsTotals))
  if (length(valid_yrs) == 0) return(NULL)
  # classes not reported in a sampled year have no fish
  ObsCounts[valid_yrs, ][is.na(ObsCounts[valid_yrs, ])] <- 0

  # Projection years hold the last year with observed data unless user-supplied
  ProjSS <- UserSS %||% matrix(ObsTotals[max(valid_yrs)], 1, nProjTS)
  CompObs@SampleSize <- cbind(matrix(ObsTotals, nrow(ProjSS), nCondYears, byrow = TRUE), ProjSS)
  dimnames(CompObs@SampleSize) <- list(Sim = seq_len(nrow(ProjSS)), Year = c(CondYears, ProjYears))

  ObsProp     <- ObsCounts / ObsTotals
  ObsProp[ObsTotals == 0 | is.na(ObsTotals), ] <- NA
  ObsPropSafe <- pmax(ObsProp, 1e-8)
  ObsPropSafe <- AddDimension(ObsPropSafe, 'Sim', pos = 1) |>
    ExtendSims(nSim = nSim) |> .Aperm(c(1,3,2))

  # Predicted proportions [nSim x nBin x nCondYear]
  PredTotals   <- apply(PredCatch, c(1,3), sum)
  PredTotals[PredTotals <= 0 | !is.finite(PredTotals)] <- NA
  PredProp     <- sweep(PredCatch, c(1,3), PredTotals, '/')
  PredPropSafe <- pmax(PredProp, 1e-8)

  # Shift: log(obs_prop[t,b]) - log(pred_prop[s,t,b])
  Resid <- ExtendSims(log(ObsPropSafe), nSim = nSim) - log(PredPropSafe)
  for (yr in seq_len(nCondYears)) {
    if (!yr %in% valid_yrs) Resid[, , yr] <- NA
    Resid[is.na(PredTotals[, yr]), , yr]  <- NA
  }

  # last conditioning year with observed data and a prediction, per sim
  last_valid_idx <- vapply(seq_len(nSim), function(s) {
    idx <- which((seq_len(nCondYears) %in% valid_yrs) & !is.na(PredTotals[s, ]))
    if (length(idx) == 0) return(NA_integer_)
    max(idx)
  }, integer(1))

  ShiftArray <- array(0, dim = c(nSim, nYearsAll, nBin),
                      dimnames = stats::setNames(list(seq_len(nSim), YearsAll, BinNames),
                                                 c('Sim', 'Year', BinDim)))
  yr_ind_all <- match(as.character(CondYears), as.character(YearsAll))
  ShiftArray[, yr_ind_all, ] <- .Aperm(Resid, c(1, 3, 2))

  # Projection years and hist years beyond conditioning hold the last valid conditioning year
  last_cond_col <- yr_ind_all[last_valid_idx]
  last_shift <- t(vapply(seq_len(nSim), function(s) {
    if (is.na(last_cond_col[s])) return(rep(NA_real_, nBin))
    ShiftArray[s, last_cond_col[s], ]
  }, numeric(nBin)))
  fill_ind <- setdiff(seq_len(nYearsAll), yr_ind_all)
  if (length(fill_ind) > 0)
    ShiftArray[, fill_ind, ] <- array(rep(last_shift, length(fill_ind)),
                                      dim = c(nSim, length(fill_ind), nBin))

  ProjInd <- match(as.character(ProjYears), as.character(YearsAll))
  if (!is.null(UserShift))
    ShiftArray[, ProjInd, ] <- .PopulateObsShift(UserShift, nSim, YearsAll, BinNames,
                                                 BinDim)[, ProjInd, , drop = FALSE]
  CompObs@Shift <- ShiftArray

  # ESS
  ObsPropExp <- AddDimension(ObsProp, 'Sim', pos = 1) |>
    ExtendSims(nSim = nSim) |> .Aperm(c(1, 3, 2))
  HN_num <- apply(PredProp * (1 - PredProp), c(1, 3), sum, na.rm = TRUE)
  HN_den <- apply((ObsPropExp - PredProp)^2, c(1, 3), sum, na.rm = TRUE)
  NHat <- HN_num / HN_den
  NHat[, !seq_len(nCondYears) %in% valid_yrs] <- NA
  # Inf (exact fit) is kept so it caps at the sample size below
  NHat[is.na(NHat) | NHat <= 0] <- NA
  NHat <- pmin(NHat, matrix(rep(ObsTotals, each = nSim), nrow = nSim, ncol = nCondYears),
               na.rm = FALSE)

  ESSArray <- array(NA_real_, dim = c(nSim, nYearsAll),
                    dimnames = list(Sim = seq_len(nSim), Year = YearsAll))
  ESSArray[, yr_ind_all] <- NHat
  # Years without an estimate take the nearest earlier valid year, else the first valid year
  ESSArray[] <- t(apply(ESSArray, 1, \(ess) rev(.CarryForwardLOCF(rev(.CarryForwardLOCF(ess))))))
  if (!is.null(UserESS)) {
    ESSArray[, ProjInd] <- UserESS
  } else if (!is.null(UserSS)) {
    ESSArray[, ProjInd] <- pmin(ESSArray[, ProjInd, drop = FALSE], UserSS)
  }
  CompObs@ESS <- ESSArray
  CompObs
}
