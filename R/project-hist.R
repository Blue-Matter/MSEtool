#' Project a Hist Object Across Management Procedures
#'
#' Runs the projection loop for one or more management procedures (MPs),
#' returning a completed [mse-class] object with projection results for all
#' MPs.
#'
#' @param Hist A [hist-class] object containing the conditioned operating
#'   model and historical dynamics, as returned by [Simulate()].
#' @param MPs Character vector of MP names, or a named list of MP functions
#'   (optionally mixed with MP names). See `.ResolveMPs()`.
#' @param parallel Logical. If `TRUE`, projects each MP in parallel using a
#'   `future` plan established by [SetupParallel()]. Errors if
#'   `TRUE` and no parallel plan is active. Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param nSim Integer. If provided, reduces the number of simulations to
#'   `nSim` before projecting. If `NULL` (default), all simulations in `Hist`
#'   are used.
#' @param Reduce Logical. Reduce object size after simulation for memory
#'   efficiency? Default `TRUE`. See [ReduceDims()].
#'
#' @return A [mse-class] object containing projection results for all MPs in
#'   `MPs`.
#'
#' @keywords internal
.ProjectHist <- function(Hist,
                         MPs = NULL, 
                         parallel=FALSE, 
                         silent=FALSE, 
                         nSim=NULL, 
                         Reduce=TRUE) {

  StartTime <- Sys.time()
  Hist <- UpdateObject(Hist)
  
  .OnExit()
  .CheckClass(Hist, 'hist', 'Hist')
  MPList <- .ResolveMPs(MPs)
  MPs    <- names(MPList)
  if (!silent)
    .CheckDataOM(Hist@OM@Control$DataOM, MPList)
  
  YearsHist <- Years(Hist@OM, "Historical")
  YearsProj <- Years(Hist@OM, "Projection")
  nMPs <- length(MPs)
  parallel <- CheckParallel(parallel)

  .MsgTheme()
  .MsgStart('Project', Hist@OM, nSim, silent, nMP = nMPs, parallel = parallel)

  step <- .MsgStep("Preparing projections", "Prepared projections", silent)
  Proj <- Hist |> ReduceNSim(nSim)

  Proj <- .CheckFleetAllocation(Proj)
  Proj <- .CheckSeasonalAllocation(Proj)
  Proj <- .CheckEffortAllocation(Proj)

  Proj <- .PrepHistMisc(Proj)
  Proj <- .CheckInterimAdvice(Proj)

  Proj <- .ExtendHist(Proj, Years = c(YearsHist, YearsProj), silent = silent)
  
  Proj <- .CalcFisheryDynamics(Proj, 
                              Years = c(utils::tail(YearsHist,1)), 
                              clone = 1) 
  
  MSE <- .Hist2MSE(Proj, MPs = MPList)
  Store <- new.env(parent = baseenv())
  MSE   <- .DetachMPArrays(MSE, Store)
  SaveLog <- Proj@Log
  Proj@Log <- list()
  .MsgStepDone(step)

  mp <- 1 # initialise for debugging

  if (parallel || !is.null(Proj@OM@Control$ProjectChunks)) {
    if (parallel) CheckPackage('future')

    nSimAll    <- nSim(Proj)
    Chunks     <- .SplitSims(nSimAll, .ResolveProjectChunks(Proj, parallel))
    ProjChunks <- lapply(Chunks, \(Sims) .ChunkProj(Proj, Sims))
    ParentIDs  <- Proj@OM@Misc[c('SimIDs', 'nSimGlobal')]
    rm(Proj)
    nChunk <- length(Chunks)
    Tasks  <- expand.grid(chunk = seq_len(nChunk), mp = seq_along(MPs))
    nTask  <- nrow(Tasks)

    TaskArgs <- \(i) list(mp = Tasks$mp[i], MPName = MPs[Tasks$mp[i]],
                          MPfunction = MSE@MPs[[MPs[Tasks$mp[i]]]],
                          Proj = ProjChunks[[Tasks$chunk[i]]],
                          YearsHist = YearsHist, YearsProj = YearsProj, silent = TRUE,
                          nMP = nMPs, StopIfAllFailed = nChunk == 1)

    status <- .MsgStatus(paste0("Projecting {nMPs} MP{?s} x {nChunk} sim chunk{?s}",
                                if (parallel) " in parallel"), silent)

    Results  <- vector('list', nTask)
    Futures  <- vector('list', nTask)
    NextTask <- 1L
    NextMP   <- 1L
    
    if (parallel) {
      OldOpt <- options(future.resolved.timeout = 0)
      on.exit(options(OldOpt), add = TRUE)
    }
    while (NextMP <= nMPs) {
      if (parallel) {
        for (i in which(!vapply(Futures, is.null, logical(1)))) {
          if (future::resolved(Futures[[i]])) {
            Results[i] <- list(future::value(Futures[[i]]))
            Futures[i] <- list(NULL)
          }
        }
        while (NextTask <= nTask && .FreeWorkers() > 0) {
          Futures[[NextTask]] <- .LaunchProjectTask(TaskArgs(NextTask))
          NextTask <- NextTask + 1L
        }
      } else if (NextTask <= nTask) {
        Results[NextTask] <- list(do.call(.ProjectMPTask, TaskArgs(NextTask)))
        NextTask <- NextTask + 1L
      }

      Ind <- which(Tasks$mp == NextMP)
      if (!any(vapply(Results[Ind], is.null, logical(1)))) {
        result <- .BindChunkResults(Results[Ind], Chunks, nSimAll, ParentIDs)
        Results[Ind] <- list(NULL)
        MSE <- .MergeMPResult(MSE, result, MPs[NextMP], NextMP, YearsHist, YearsProj, silent,
                              nMP = nMPs, Store = Store)
        rm(result)
        NextMP <- NextMP + 1L
      } else if (parallel) {
        Sys.sleep(0.05)
      }
    }
    rm(ProjChunks)
    if (!is.null(status)) cli::cli_progress_done(status)

  } else {
    for (mp in seq_along(MPs)) {
      MPName <- MPs[mp]
      MPfunction <- MSE@MPs[[MPName]]

      MSE <- .ProjectMP(Proj,
                        MSE,
                        MPName,
                        MPfunction,
                        mp,
                        YearsHist,
                        YearsProj,
                        silent,
                        nMP = nMPs,
                        Store = Store)

    }
  }

  MSE <- .AttachMPArrays(MSE, Store)
  rm(Store)

  .MsgDone('Project', StartTime, silent)

  MSE <- .RestoreHistMisc(MSE)

  MSE@Log <- .JoinLog(SaveLog, MSE@Log)

  if (!silent)
    .CheckLog(MSE, 'mse')

  .ReduceMSE(MSE, Reduce) 
}

# ---- Sim chunks for parallel projection ----

# Number of sim chunks per MP: OM@Control$ProjectChunks, else one per worker (1 if sequential)
.ResolveProjectChunks <- function(Proj, parallel = TRUE) {
  K <- Proj@OM@Control$ProjectChunks %||% if (parallel) future::nbrOfWorkers() else 1L
  if (!is.numeric(K) || length(K) != 1 || is.na(K) || K < 1)
    cli::cli_abort("{.code OM@Control$ProjectChunks} must be a single positive integer.")
  max(1L, min(as.integer(K), nSim(Proj)))
}

# One (chunk, MP) projection task as a future; the worker receives only the task's arguments
.LaunchProjectTask <- function(Args) {
  future::future(do.call(Task, Args),
                 globals  = list(Task = .ProjectMPTask, Args = Args),
                 packages = "MSEtool",
                 seed     = TRUE)
}

.FreeWorkers <- function() {
  tryCatch(future::nbrOfFreeWorkers(), error = \(e) 1L)
}

# Contiguous sim chunks with sizes differing by at most one
.SplitSims <- function(nSim, K) {
  K     <- max(1L, min(as.integer(K), nSim))
  Sizes <- rep(nSim %/% K, K) + (seq_len(K) <= nSim %% K)
  unname(split(seq_len(nSim), rep(seq_len(K), Sizes)))
}

# Subset of Proj for `Sims`; OM@Misc$SimIDs maps its local sims back to the full run
.ChunkProj <- function(Proj, Sims) {
  nSimAll <- nSim(Proj)
  if (length(Sims) == nSimAll) return(Proj)
  Chunk <- .SubsetSim(Proj, Sims, nSim = nSimAll)
  Chunk@OM@Misc$SimIDs     <- .GlobalSim(Proj@OM, Sims)
  Chunk@OM@Misc$nSimGlobal <- .GlobalNSim(Proj@OM)
  Chunk
}

# Joins the per-chunk results of one MP into a single full-nSim result for .MergeMPResult()
.BindChunkResults <- function(Results, Chunks, nSimAll, ParentIDs = list()) {
  if (length(Results) == 1) return(Results[[1]])

  Sizes <- lengths(Chunks)
  Projs <- lapply(Results, `[[`, 'Proj')
  Proj  <- Projs[[1]]

  for (sl in setdiff(methods::slotNames('timeseries'), 'Misc'))
    methods::slot(Proj, sl) <- .BindSims(lapply(Projs, methods::slot, sl), Sizes)

  Proj@OM@Fleet            <- .BindSims(lapply(Projs, \(x) x@OM@Fleet), Sizes)
  Proj@OM@nSim             <- nSimAll
  Proj@OM@Misc$SimIDs      <- ParentIDs$SimIDs
  Proj@OM@Misc$nSimGlobal  <- ParentIDs$nSimGlobal
  Proj@Data                <- do.call(c, lapply(Projs, methods::slot, 'Data'))

  for (nm in c('MPAdvice', 'MPAggBagLimit')) {
    Years <- names(Proj@Misc[[nm]])
    for (yr in Years) {
      SimList <- do.call(c, lapply(Projs, \(x) x@Misc[[nm]][[yr]]))
      if (length(SimList) == nSimAll) names(SimList) <- seq_len(nSimAll)
      Proj@Misc[[nm]][[yr]] <- SimList
    }
  }

  Errors    <- vapply(Results, `[[`, logical(1), 'Error')
  AllFailed <- Reduce(intersect, lapply(Results, `[[`, 'AllFailedYears'))
  CutYear   <- min(c(Inf, unlist(lapply(Results[Errors], `[[`, 'ErrorYear')), AllFailed))

  Error        <- is.finite(CutYear)
  ErrorMessage <- NULL
  if (Error) {
    First <- which(Errors & vapply(Results, \(r) identical(r$ErrorYear, CutYear), logical(1)))
    ErrorMessage <- if (length(First)) Results[[First[1]]]$ErrorMessage else
      sprintf("failed for all simulations in %s", CutYear)
  }

  # an unchunked run stops at CutYear; drop what chunks logged after it
  Proj@Log <- .BindChunkLogs(lapply(Projs, methods::slot, 'Log'), Chunks, CutYear)

  list(Proj         = Proj,
       Error        = Error,
       ErrorMessage = ErrorMessage,
       StartTime    = do.call(min, lapply(Results, `[[`, 'StartTime')),
       EndTime      = do.call(max, lapply(Results, `[[`, 'EndTime')),
       StockNames   = Results[[1]]$StockNames,
       FleetNames   = Results[[1]]$FleetNames)
}

# Binds matching arrays from each chunk along `Sim`; recurses into S4 objects and lists
.BindSims <- function(Pieces, Sizes) {
  x <- Pieces[[1]]
  if (is.null(x)) return(x)

  if (isS4(x)) {
    for (sl in methods::slotNames(x)) {
      Vals <- lapply(Pieces, methods::slot, sl)
      if (!is.null(Vals[[1]]))
        methods::slot(x, sl, check = FALSE) <- .BindSims(Vals, Sizes)
    }
    if ('nSim' %in% methods::slotNames(x)) x@nSim <- sum(Sizes)
    return(x)
  }

  if (is.list(x) && !is.data.frame(x)) {
    for (i in seq_along(x))
      if (!is.null(x[[i]])) x[[i]] <- .BindSims(lapply(Pieces, `[[`, i), Sizes)
    return(x)
  }

  DN <- names(dimnames(x))
  if (!is.array(x) || !'Sim' %in% DN) return(x)

  Along <- match('Sim', DN)
  nPer  <- vapply(Pieces, \(p) dim(p)[Along], numeric(1))
  if (all(nPer == 1) && all(vapply(Pieces[-1], identical, logical(1), x)))
    return(x)

  Pieces <- Map(\(p, n) if (dim(p)[Along] == 1 && n > 1) ExtendSims(p, n) else p, Pieces, Sizes)
  out <- abind::abind(Pieces, along = Along)
  dimnames(out)[[Along]] <- seq_len(sum(Sizes))
  names(dimnames(out)) <- DN
  out
}

# Concatenates chunk logs, mapping each entry's local sim to its global sim
.BindChunkLogs <- function(Logs, Chunks, CutYear = Inf) {
  Types <- unique(unlist(lapply(Logs, names)))
  out <- list()
  for (type in Types) {
    Entries <- do.call(c, Map(\(Log, Sims) {
      lapply(Log[[type]], \(e) {
        if (.IsLogEntry(e) && !is.null(e$sim)) e$sim <- Sims[e$sim]
        e
      })
    }, Logs, Chunks))
    if (!length(Entries)) next
    NoSim   <- vapply(Entries, \(e) !.IsLogEntry(e) || is.null(e$sim), logical(1))
    Entries <- Entries[!(NoSim & duplicated(Entries))]
    Year    <- vapply(Entries, \(e) if (.IsLogEntry(e) && length(e$year)) as.numeric(e$year)[1] else -Inf, numeric(1))
    Keep    <- Year <= CutYear
    if (any(Keep)) out[[type]] <- Entries[Keep][order(Year[Keep])]
  }
  out
}
