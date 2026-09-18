
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

  AnyActive <- any(purrr::map_lgl(Groups, \(g)
    any(purrr::map_lgl(g$sources, \(s) length(Hist@OM@Stock[[s]]@Depletion@Final) > 0))
  ))
  if (!AnyActive) return(Hist)

  Hist@OM <- .CheckCatchFrac(Hist@OM)

  nStock <- nStock(Hist@OM)
  nFleet <- nFleet(Hist@OM)
  nSim <- Hist@OM@nSim
  nArea <- nArea(Hist)
  YearsHist <- Years(Hist@OM, 'Historical')

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
  } else if (silent) {
    lapply(HistSim_List, .OptFinalDepletionSim, nStock, nFleet, nArea, YearsHist, Groups)
  } else {
    purrr::map(HistSim_List, \(HistSim) {
      .OptFinalDepletionSim(HistSim, nStock, nFleet, nArea, YearsHist, Groups)
    }, .progress = list(
      type = "iterator",
      format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
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
  if (!silent) cli::cli_alert_success("Optimized catchability (q) for Final Depletion")

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


.OptFinalDepletionSim <- function(HistSim, nStock, nFleet, nArea, YearsHist, Groups) {

  bounds <- c(1e-02, 3)
  tol <- 1E-5
  silent <- TRUE

  ResolvedGroups <- purrr::map(Groups, .ResolveGroupTarget, OM = HistSim@OM)
  ActiveInfo <- .ActiveGroupInfo(ResolvedGroups)

  if (!length(ActiveInfo$sources))
    return(HistSim@OM@Fleet)

  if (nStock > 1 || nFleet > 1) {
    pars <- .OptimizeCatchabilityMulti(HistSim, nStock, nFleet,
                                       nArea, YearsHist, bounds, tol,
                                       silent, ActiveInfo)
  } else {
    pars <- .OptimizeCatchabilitySingle(HistSim, nStock, nFleet,
                                        nArea, YearsHist, bounds, tol,
                                        silent, ActiveInfo)
  }

  nSrc <- length(ActiveInfo$sources)
  qSource <- exp(pars[seq_len(nSrc)])
  qStock  <- .DeriveStockQ(qSource, ActiveInfo, nStock)
  qFleet  <- matrix(1, nStock, nFleet)

  if (nFleet > 1) {
    nAct <- length(ActiveInfo$stocks)
    qlogit <- matrix(0, nStock, nFleet)
    qlogit[ActiveInfo$stocks, 2:nFleet] <- matrix(pars[(nSrc+1):length(pars)], nrow = nAct)
    qFleet <- ilogitm(qlogit)
  }

  for (st in ActiveInfo$stocks) {
    for (fl in 1:nFleet) {
      StCatchability <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
      StCatchability <- StCatchability/StCatchability[1]
      HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency <- StCatchability * qStock[st] * qFleet[st,fl]
    }
  }

  HistSim@OM@Fleet
}

.OptimizeCatchabilitySingle <- function(HistSim, nStock, nFleet, nArea, YearsHist, bounds, tol, silent, ActiveInfo, debug=FALSE) {

  doOpt <- stats::optimize(.OptCatchability,
                           log(bounds),
                           Hist=HistSim,
                           nStock=nStock,
                           nFleet=nFleet,
                           nArea=nArea,
                           YearsHist=YearsHist,
                           ActiveInfo=ActiveInfo,
                           debug=debug,
                           tol=tol)
  pars <- doOpt$minimum

  if (any(abs(exp(pars) - bounds) < 0.01)) {
    # more robust than optimize but slower
    doOpt <- stats::nlminb(mean(log(bounds)),
                           .OptCatchability,
                           Hist=HistSim,
                           nStock=nStock,
                           nFleet=nFleet,
                           nArea=nArea,
                           YearsHist=YearsHist,
                           ActiveInfo=ActiveInfo,
                           debug=debug,
                           lower=log(bounds[1]),
                           upper=log(bounds[2]))
    pars <- doOpt$par
  }
  pars

}

.OptCatchability <- function(pars, HistSim, nStock, nFleet, nArea, YearsHist, ActiveInfo, CatchFrac=NULL, debug=FALSE) {

  nSrc <- length(ActiveInfo$sources)
  qSource <- exp(pars[seq_len(nSrc)])
  qStock  <- .DeriveStockQ(qSource, ActiveInfo, nStock)
  qFleet  <- matrix(1, nStock, nFleet)

  if (nFleet > 1) {
    nAct <- length(ActiveInfo$stocks)
    qlogit <- matrix(0, nStock, nFleet)
    qlogit[ActiveInfo$stocks, 2:nFleet] <- matrix(pars[(nSrc+1):length(pars)], nrow = nAct)
    qFleet <- ilogitm(qlogit)
  }

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
                                                DoCalcCatch=DoCalcCatch,
                                                clone=1)


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

  cOBJ <- sum(log(CatchFrac/predCatchFrac)^2)
  depOBJ <- depOBJ+cOBJ

  depOBJ

}


.OptimizeCatchabilityMulti <- function(HistSim, nStock, nFleet, nArea,
                                       YearsHist, bounds, tol, silent, ActiveInfo, debug=FALSE) {

  CalcCatchFrac <- FALSE
  if (is.null(HistSim@OM@CatchFrac))
    CalcCatchFrac <- TRUE

  if (is.list(HistSim@OM@CatchFrac) && any(lapply(HistSim@OM@CatchFrac, is.null) |>
                                           unlist()))
    CalcCatchFrac <- TRUE

  # Catch divided by effort (q proxy)
  if (CalcCatchFrac) {
    HistSim@OM@CatchFrac <- MakeNamedList(StockNames(HistSim@OM))
    for (st in 1:nStock) {
      relF <- rep(NA, nFleet)
      for (fl in 1:nFleet) {
        effort <- HistSim@OM@Fleet[[st]][[fl]]@Effort@Effort
        q <- HistSim@OM@Fleet[[st]][[fl]]@Catchability@Efficiency
        relF[fl] <- effort[1,ncol(effort)] * q[1,ncol(q)]
      }
      HistSim@OM@CatchFrac[[st]] <- relF/sum(relF)
    }
  }

  CatchFrac <- List2Array(HistSim@OM@CatchFrac, name = 'Stock', dim1 = 'Fleet', pos=2) |>
    t()

  EffortFleet <- array(NA, dim=c(nStock, nFleet))
  nTS <- length(YearsHist)
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      EffortFleet[st,fl] <- HistSim@OM@Fleet[[st]][[fl]]@Effort@Effort[1,nTS]
    }

  }

  FDist <- CatchFrac/EffortFleet
  FDist[!is.finite(FDist)] <- tiny
  FDist <- FDist/apply(FDist[, , drop = FALSE], 1, sum)    # q ratio proxy (real space)

  nSrc <- length(ActiveInfo$sources)
  nAct <- length(ActiveInfo$stocks)

  lower <- c(rep(log(bounds[1]), nSrc), rep(-5, nAct * (nFleet-1)))
  upper <- c(rep(log(bounds[2]), nSrc), rep(5,  nAct * (nFleet-1)))

  if (nFleet == 1) {
    pars <- rep(-5, nSrc)
  } else {
    # low initial F followed by logit guess at fraction based on Fdist
    # according to catch fraction in recent year
    pars <- c(rep(-5, nSrc), logit(FDist[ActiveInfo$stocks, 2:nFleet, drop=FALSE]))
  }

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
                 CatchFrac = CatchFrac,
                 debug=debug,
                 control = list(trace = ifelse(silent, 0, 1), factr = tol/.Machine$double.eps)
  )
  pars <- doOpt$par
  pars
}

