
#' Optimize Catchability to Match Final Depletion
#'
#' Internal function to adjust fleet-specific catchability (q) to match
#' the target final depletion for each stock in the historical operating model.
#'
#'
#' @param Hist A `Hist` object.
#' @param parallel Logical; if `TRUE`, optimizes catchability across
#'   simulations in parallel using a `future` plan established by
#'   [SetupParallel()]. Default `FALSE`.
#' @param silent Logical; if `TRUE`, suppress progress output.
#'
#' Stocks are grouped by `SRR@SPFrom` connectivity: within each
#' group, self-recruiting ("source") stocks each get an independently-fit
#' catchability scale, while `SPFrom`-dependent stocks' scale is derived as
#' the weighted average of their source(s)' fitted scale. The depletion
#' objective is evaluated once per group, against a target taken from the
#' group's source stock(s) only; a dependent's own `Depletion@Final`/
#' `Reference` (if set) has no effect.
#' 
#' The function loops over all simulations (`nSim`) in `Hist` and performs
#' numeric optimization to scale the initial catchability to achieve the
#' desired final depletion specified in `Stock@Depletion@Final`.
#'
#' Uses `stats::optimize` with a fallback to `nlminb` if results are
#' near bounds.
#'
#' @return A modified [Hist()] object with optimized `Catchability@Efficiency`
#'   in each fleet of the operating model.
#'
#' @keywords internal
.OptFinalDepletion <- function(Hist, parallel=FALSE, silent=FALSE) {

  Groups <- .SPFromConnectedGroups(Hist@OM)
  .CheckSelfBlendDepletion(Groups, Hist@OM)

  AnyActive <- any(purrr::map_lgl(Groups, \(g)
    any(purrr::map_lgl(g$sources, \(s) length(Hist@OM@Stock[[s]]@Depletion@Final) > 0))
  ))
  if (!AnyActive) return(Hist)

  .MsgStep("Optimizing catchability (q) for final depletion",
           "Optimized catchability (q) for final depletion", silent)

  Hist@OM <- .CheckCatchFrac(Hist@OM)

  nStock <- nStock(Hist@OM)
  nFleet <- nFleet(Hist@OM)
  nSim <- Hist@OM@nSim
  nArea <- nArea(Hist)
  YearsHist <- Years(Hist@OM, 'Historical')

  if (nFleet > 1)
    Hist <- .CheckTerminalEffort(Hist, nStock, nFleet, nSim)

  # List length nSim, each with a Hist object with 1 sim
  HistSim_List <- lapply(seq_len(nSim), function(i) Subset(Hist, i))

  # HistSim <- HistSim_List[[1]] # for debugging

  parallel <- CheckParallel(parallel)

  opt_q <- if (parallel) {
    CheckPackage('furrr')
    furrr::future_map(
      HistSim_List, .OptFinalDepletionSim, nStock, nFleet, nArea, YearsHist, Groups,
      .options = furrr::furrr_options(
        globals  = c('nStock', 'nFleet', 'nArea', 'YearsHist', 'Groups'),
        packages = "MSEtool",
        seed     = 101
      )
    )
  } else if (!.MsgShowProgress(silent)) {
    lapply(HistSim_List, .OptFinalDepletionSim, nStock, nFleet, nArea, YearsHist, Groups)
  } else {
    purrr::map(HistSim_List, \(HistSim) {
      .OptFinalDepletionSim(HistSim, nStock, nFleet, nArea, YearsHist, Groups)
    }, .progress = list(
      type = "iterator",
      format = "Optimizing catchability (q) for final depletion {cli::pb_bar} {cli::pb_percent}",
      clear = TRUE))
  }


  # Update Hist object
  for (s in seq_along(opt_q)) {
    for (st in 1:nStock) {
      for (fl in 1:nFleet) {
        Hist@OM@Fleet[[st]][[fl]]@Catchability@Efficiency[s,] <- opt_q[[s]][[st]][[fl]]@Catchability@Efficiency[1,]
        Hist@Misc$Catchability[s,st,, fl] <- opt_q[[s]][[st]][[fl]]@Catchability@Efficiency[1,]
      }
    }
  }
  Hist
}


.CheckTerminalEffort <- function(Hist, nStock, nFleet, nSim) {
  FleetNames <- FleetNames(Hist@OM)
  StockNames <- StockNames(Hist@OM)
  Bad <- NoEffort <- character(0)

  for (st in 1:nStock) {
    TermEffort <- vapply(1:nFleet, \(fl) {
      Effort <- Hist@OM@Fleet[[st]][[fl]]@Effort@Effort
      rep_len(Effort[, ncol(Effort)], nSim)
    }, numeric(nSim)) |> matrix(nSim, nFleet)

    CF <- Hist@OM@CatchFrac[[st]]
    for (fl in 1:nFleet) {
      Zero <- which(TermEffort[, fl] == 0)
      if (length(Zero))
        NoEffort <- c(NoEffort, cli::format_inline("{.val {FleetNames[fl]}} ({.val {StockNames[st]}}): {cli::qty(length(Zero))}Sim{?s} {Zero}"))
      if (is.null(CF)) next
      CFfl <- rep_len(CF[, fl], nSim)
      Wrong <- which((CFfl > 0) != (TermEffort[, fl] > 0))
      if (length(Wrong))
        Bad <- c(Bad, cli::format_inline("{.val {FleetNames[fl]}} ({.val {StockNames[st]}}): {cli::qty(length(Wrong))}Sim{?s} {Wrong}"))
    }
  }

  if (length(Bad))
    cli::cli_abort(c(
      'x' = '`OM@CatchFrac` must be zero for fleets with zero effort in the final historical year, and positive otherwise:',
      stats::setNames(Bad, rep('*', length(Bad)))
    ), call = NULL)

  if (length(NoEffort)) {
    Hist <- .CaptureLog(Hist,
                        string = cli::format_inline("Effort is zero in the final historical year for: {paste(NoEffort, collapse = '; ')}"),
                        name = 'Catchability',
                        type = 'assumption')
    Hist <- .CaptureLog(Hist,
                        string = "Catchability of these fleets is set relative to the first fleet with final-year effort, by the ratio of their `Catchability@Efficiency`",
                        type = 'assumption')
  }
  Hist
}

.ResolveGroupTarget <- function(group, OM) {
  srcs <- group$sources
  finals <- purrr::map(srcs, \(s) OM@Stock[[s]]@Depletion@Final)
  has_final <- purrr::map_lgl(finals, \(f) length(f) > 0)

  if (!any(has_final))
    return(c(group, list(active = FALSE, value = NA_real_, reference = NA_character_)))

  set_srcs <- srcs[has_final]
  vals <- purrr::map_dbl(set_srcs, \(s) as.numeric(OM@Stock[[s]]@Depletion@Final))
  refs <- purrr::map_chr(set_srcs, \(s) OM@Stock[[s]]@Depletion@Reference)

  pairs <- paste(vals, refs, sep = "|")
  if (length(unique(pairs)) > 1)
    cli::cli_abort(c(
      "x" = "Stocks {.val {StockNames(OM)[set_srcs]}} are linked via {.field SRR@SPFrom} (as sources of a shared dependent) but specify conflicting {.field Depletion@Final}/{.field Depletion@Reference} targets.",
      "i" = "Only one distinct target/reference may be specified among the source stocks of an `SPFrom`-linked group."
    ))

  c(group, list(active = TRUE, value = vals[1], reference = refs[1]))
}

.CheckSelfBlendDepletion <- function(Groups, OM) {
  for (g in Groups) {
    self_blend <- g$dependents[purrr::map2_lgl(g$dependents, g$weights, \(d, w) d %in% w$from)]
    if (!length(self_blend)) next
    has_final <- purrr::map_lgl(g$members, \(s) length(OM@Stock[[s]]@Depletion@Final) > 0)
    if (any(has_final))
      cli::cli_abort(c(
        "x" = "{.field Depletion@Final} cannot be fitted for stock{?s} {.val {StockNames(OM)[self_blend]}}: {.field SRR@SPFrom} blends the stock's own spawning production with another stock's.",
        "i" = "Remove {.field Depletion@Final} from stocks {.val {StockNames(OM)[g$members]}} and set {.field Catchability@Efficiency} directly."
      ))
  }
  invisible(TRUE)
}

.ActiveGroupInfo <- function(ResolvedGroups) {
  active <- purrr::keep(ResolvedGroups, \(g) g$active)
  list(
    groups  = active,
    sources = unlist(lapply(active, `[[`, "sources")),
    stocks  = sort(unlist(lapply(active, `[[`, "members")))
  )
}

.DeriveStockQ <- function(qSource, ActiveInfo, nStock) {
  qStock <- rep(1, nStock)
  idx <- 1
  for (g in ActiveInfo$groups) {
    for (s in g$sources) {
      qStock[s] <- qSource[idx]
      idx <- idx + 1
    }
  }
  for (g in ActiveInfo$groups) {
    for (di in seq_along(g$dependents)) {
      d <- g$dependents[di]
      w <- g$weights[[di]]
      qStock[d] <- sum(qStock[w$from] * w$weight) / sum(w$weight)
    }
  }
  qStock
}


.TerminalEffortFleet <- function(OM, nStock, nFleet) {
  EffortFleet <- matrix(NA_real_, nStock, nFleet)
  for (st in 1:nStock)
    for (fl in 1:nFleet) {
      Effort <- OM@Fleet[[st]][[fl]]@Effort@Effort
      EffortFleet[st, fl] <- Effort[1, ncol(Effort)]
    }
  EffortFleet
}

# Which fleet q-shares are estimated, and fixed log-offsets for fleets with no terminal effort
.QFleetMap <- function(HistSim, nStock, nFleet, ActiveInfo) {
  Active <- .TerminalEffortFleet(HistSim@OM, nStock, nFleet) > 0
  Est    <- matrix(FALSE, nStock, nFleet)
  Offset <- matrix(0, nStock, nFleet)
  Ref    <- rep(1L, nStock)
  for (st in ActiveInfo$stocks) {
    Ref[st] <- which(Active[st, ])[1] %||NA% 1L
    Est[st, ] <- Active[st, ]
    Est[st, Ref[st]] <- FALSE
    Eff1 <- vapply(1:nFleet, \(fl) HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency[1], numeric(1))
    Fixed <- !Active[st, ]
    Offset[st, Fixed] <- log(Eff1[Fixed] / Eff1[Ref[st]])
  }
  list(Active = Active, Est = Est, Offset = Offset, Ref = Ref)
}

.QFleet <- function(pars, nSrc, nStock, nFleet, QMap) {
  if (nFleet == 1) return(matrix(1, nStock, nFleet))
  qlogit <- QMap$Offset
  if (any(QMap$Est))
    qlogit[QMap$Est] <- pars[(nSrc + 1):length(pars)]
  ilogitm(qlogit)
}

.OptFinalDepletionSim <- function(HistSim, nStock, nFleet, nArea, YearsHist, Groups) {

  bounds <- c(1e-02, 3)
  tol <- 1E-5
  silent <- TRUE

  ResolvedGroups <- purrr::map(Groups, .ResolveGroupTarget, OM = HistSim@OM)
  ActiveInfo <- .ActiveGroupInfo(ResolvedGroups)

  if (!length(ActiveInfo$sources))
    return(HistSim@OM@Fleet)

  QMap <- .QFleetMap(HistSim, nStock, nFleet, ActiveInfo)

  if (nStock > 1 || nFleet > 1) {
    pars <- .OptimizeCatchabilityMulti(HistSim, nStock, nFleet,
                                       nArea, YearsHist, bounds, tol,
                                       silent, ActiveInfo, QMap)
  } else {
    pars <- .OptimizeCatchabilitySingle(HistSim, nStock, nFleet,
                                        nArea, YearsHist, bounds, tol,
                                        silent, ActiveInfo, QMap)
  }

  nSrc <- length(ActiveInfo$sources)
  qSource <- exp(pars[seq_len(nSrc)])
  qStock  <- .DeriveStockQ(qSource, ActiveInfo, nStock)
  qFleet  <- .QFleet(pars, nSrc, nStock, nFleet, QMap)

  for (st in ActiveInfo$stocks) {
    for (fl in 1:nFleet) {
      StCatchability <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
      StCatchability <- StCatchability/StCatchability[1]
      HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency <- StCatchability * qStock[st] * qFleet[st,fl]
    }
  }

  HistSim@OM@Fleet
}

.OptimizeCatchabilitySingle <- function(HistSim, nStock, nFleet, nArea, YearsHist, bounds, tol, silent, ActiveInfo, QMap, debug=FALSE) {

  doOpt <- stats::optimize(.OptCatchability,
                           log(bounds),
                           HistSim=HistSim,
                           nStock=nStock,
                           nFleet=nFleet,
                           nArea=nArea,
                           YearsHist=YearsHist,
                           ActiveInfo=ActiveInfo,
                           QMap=QMap,
                           debug=debug,
                           tol=tol)
  pars <- doOpt$minimum

  if (any(abs(exp(pars) - bounds) < 0.01)) {
    # more robust than optimize but slower
    doOpt <- stats::nlminb(mean(log(bounds)),
                           .OptCatchability,
                           HistSim=HistSim,
                           nStock=nStock,
                           nFleet=nFleet,
                           nArea=nArea,
                           YearsHist=YearsHist,
                           ActiveInfo=ActiveInfo,
                           QMap=QMap,
                           debug=debug,
                           lower=log(bounds[1]),
                           upper=log(bounds[2]))
    pars <- doOpt$par
  }
  pars

}

.OptCatchability <- function(pars, HistSim, nStock, nFleet, nArea, YearsHist, ActiveInfo, QMap, CatchFrac=NULL, debug=FALSE) {

  nSrc <- length(ActiveInfo$sources)
  qSource <- exp(pars[seq_len(nSrc)])
  qStock  <- .DeriveStockQ(qSource, ActiveInfo, nStock)
  qFleet  <- .QFleet(pars, nSrc, nStock, nFleet, QMap)

  for (st in ActiveInfo$stocks) {
    for (fl in 1:nFleet) {
      StCatchability <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
      StCatchability <- StCatchability/StCatchability[1]

      # update Misc used in C++
      HistSim@Misc$Catchability[1,st,,fl] <- StCatchability * qStock[st] * qFleet[st,fl]
    }
  }

  DoCalcCatch <- 0
  if (nFleet>1) DoCalcCatch <- 1

  PopDynamicsHistorical <- CalcFisheryDynamics_(HistSim,
                                                Years=YearsHist,
                                                AllYears=YearsHist,
                                                Sims=1,
                                                nSim=1,
                                                nStock,
                                                nFleet,
                                                nArea,
                                                DoCalcCatch=DoCalcCatch)


  # Depletion objective: once per SPFrom-connected group
  TermInd <- length(YearsHist)

  depOBJ <- 0
  for (g in ActiveInfo$groups) {
    if (g$reference == 'B0') {
      predNum <- sum(PopDynamicsHistorical@Biomass[1, g$members, TermInd])
      refNum  <- sum(HistSim@Unfished@Equilibrium@Biomass[1, g$members, TermInd])
    } else if (g$reference == 'SB0') {
      predNum <- sum(PopDynamicsHistorical@SBiomass[1, g$sources, TermInd])
      refNum  <- sum(HistSim@Unfished@Equilibrium@SBiomass[1, g$sources, TermInd])
    } else {
      cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")
    }
    depOBJ <- depOBJ + log((predNum/refNum) / g$value)^2
  }

  if (nFleet==1) {
    return(depOBJ)
  }

  # Catch objective
  predCatchFrac <- PopDynamicsHistorical@Landings[1,,length(YearsHist), ,drop=FALSE] |>
    abind::adrop(c(1,3))

  total <- matrix(apply(predCatchFrac, 1, sum), nrow=nStock, ncol=nFleet)
  total[total==0] <- tiny
  predCatchFrac <- predCatchFrac/total

  # fleets with no terminal effort carry no information on the catch split
  Use  <- QMap$Active
  cOBJ <- sum(log(CatchFrac[Use]/predCatchFrac[Use])^2)
  depOBJ <- depOBJ+cOBJ

  depOBJ

}


.OptimizeCatchabilityMulti <- function(HistSim, nStock, nFleet, nArea,
                                       YearsHist, bounds, tol, silent, ActiveInfo, QMap, debug=FALSE) {

  EffortFleet <- .TerminalEffortFleet(HistSim@OM, nStock, nFleet)

  # Catch divided by effort (q proxy) where CatchFrac is not specified
  CatchFrac <- matrix(NA_real_, nStock, nFleet)
  for (st in 1:nStock) {
    CF <- HistSim@OM@CatchFrac[[st]]
    if (!is.null(CF)) {
      CatchFrac[st, ] <- if (is.matrix(CF)) CF[1, ] else CF
      next
    }
    relF <- vapply(1:nFleet, \(fl) {
      q <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
      EffortFleet[st, fl] * q[1, ncol(q)]
    }, numeric(1))
    CatchFrac[st, ] <- if (sum(relF) > 0) relF/sum(relF) else 0
  }

  FDist <- CatchFrac/EffortFleet
  FDist[!is.finite(FDist)] <- tiny
  FDist <- FDist/apply(FDist[, , drop = FALSE], 1, sum)    # q ratio proxy (real space)

  nSrc <- length(ActiveInfo$sources)
  nEst <- sum(QMap$Est)

  lower <- c(rep(log(bounds[1]), nSrc), rep(-5, nEst))
  upper <- c(rep(log(bounds[2]), nSrc), rep(5,  nEst))

  # low initial F followed by logit guess at fraction based on Fdist
  # according to catch fraction in recent year
  pars <- c(rep(-5, nSrc), logit(FDist[QMap$Est]))

  # a near-zero catch fraction gives logit ~ -34, far outside the box
  pars <- pmin(pmax(pars, lower), upper)

  doOpt <- optim(pars,
                 .OptCatchability,
                 method = "L-BFGS-B",
                 lower = lower,
                 upper = upper,
                 HistSim = HistSim,
                 nStock = nStock,
                 nFleet = nFleet,
                 nArea = nArea,
                 YearsHist=YearsHist,
                 ActiveInfo = ActiveInfo,
                 QMap = QMap,
                 CatchFrac = CatchFrac,
                 debug=debug,
                 control = list(trace = ifelse(silent, 0, 1), factr = tol/.Machine$double.eps)
  )
  pars <- doOpt$par
  pars
}

