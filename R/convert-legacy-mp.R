#' Wrap a Legacy `"MP"`-Class Function for the New MP Framework
#'
#' Wraps a legacy (DLMtool/SAMtool) management procedure - `class(fn) <-
#' "MP"`, signature `function(x, Data, reps, plot)`, consuming a legacy
#' [Data-legacy-class] object and returning a [Rec-class] object - so it can
#' be called by the new `om`/`hist` projection pipeline, which calls MPs as
#' `function(Data)` on a [data-class] object and expects an [advice-class]
#' object back.
#'
#' The wrapped function converts `Data` (new `data`-class) to a legacy `Data`
#' object with `nsim = 1` via [Convert()], calls `MP` with `x = 1` and
#' `reps = 1` (a single draw, since `advice@TAC`/`advice@Effort` expect a
#' single value rather than the distribution over `reps` legacy MPs use to
#' express implementation uncertainty), then maps the returned `Rec` onto an
#' `advice` object via `.Rec2Advice()`.
#'
#' If `MP` needs a `Data` slot that [Convert()] could not populate from the
#' source `data` object (see `Data@Misc$UnmappedSlots`), the call to `MP`
#' will typically error; this is caught by the caller
#' (`.CalcAdviceSimMP()`) and logged rather than aborting the run.
#'
#' @param MP A function of class `"MP"`.
#'
#' @return A function of class `"mp"` wrapping `MP`.
#' @keywords internal
.WrapLegacyMP <- function(MP) {
  # MP is stored as a literal default so it survives `.MakeSelfContained()` replacing the environment
  wrapped <- function(Data, .LegacyMP) {
    LegacyData <- Convert(Data, silent = TRUE)
    Rec <- tryCatch(
      .LegacyMP(x = 1, Data = LegacyData, reps = 1, plot = FALSE),
      error = \(e) .LegacyMPError(e, LegacyData)
    )
    .Rec2Advice(Rec, LegacyData)
  }
  f <- formals(wrapped)
  f$.LegacyMP <- MP
  formals(wrapped) <- f

  attr(wrapped, 'LegacyMP') <- MP
  class(wrapped) <- 'mp'
  wrapped
}

# legacy Data life-history slots read by DLMtool/SAMtool MPs
.LegacyLifeHistorySlots <- c('Mort', 'vbK', 'vbLinf', 'vbt0', 'L50', 'L95',
                             'wla', 'wlb', 'steep', 'MaxAge')

.LegacyMPError <- function(e, LegacyData) {
  Slots <- .LegacyLifeHistorySlots[vapply(.LegacyLifeHistorySlots, \(sl)
    methods::.hasSlot(LegacyData, sl) && all(is.na(methods::slot(LegacyData, sl))), logical(1))]
  msg <- conditionMessage(e)
  if (length(Slots))
    msg <- paste0(msg, " (legacy `Data` life-history slots ", paste0("`", Slots, "`", collapse = ", "),
                  " are NA: life-history data are not generated for simulated data unless the Obs of the OM sets LifeHistoryObs())")
  stop(msg, call. = FALSE)
}

.RecSet <- function(x) length(x) > 0 && !all(is.na(x))

# legacy double-normal retention (`LR5`, `LFR`, `Rmaxlen`) with an upper harvest slot `HS`
.LegacySlotRetention <- function(Length, LR5, LFR, HS, Rmaxlen = 1) {
  Ret <- DoubleNormalRetention(Length, LR5, LFR, Rmaxlen)
  Ret[Length >= HS] <- 0
  Ret
}

#' Convert a Legacy `Rec` Object to an `advice` Object
#'
#' Maps the slots of a legacy [Rec-class] recommendation (returned by a
#' `class(fn) <- "MP"` management procedure, see `.WrapLegacyMP()`) onto the
#' equivalent [advice-class] slots used by the new MP framework. Only slots
#' the legacy MP actually populated are set; everything else remains `NULL`
#' (unchanged from the previous timestep), matching `Rec`'s own "only some
#' slots set" behaviour. `Rec@LR5`/`LFR`/`Rmaxlen` map to [DoubleNormalRetention()]
#' (with an upper harvest slot when `Rec@HS` is set) and `Rec@L5`/`LFS`/`Vmaxlen`
#' to [DoubleNormal()] selectivity. `Rec@Fdisc`/`Rec@DR` (discard mortality) have no
#' mapping in v1 and are recorded as an assumption if set. Any `Log` entries
#' recorded on `LegacyData` (e.g. by [Convert()]/`.ConvertDataToLegacy()`) are folded
#' into the returned `Advice@Log`.
#'
#' @param Rec A [Rec-class] object returned by a legacy MP.
#' @param LegacyData The legacy [Data-legacy-class] object the MP was called
#'   with (used only to carry forward any `Log` entries from conversion).
#'
#' @return An [advice-class] object.
#' @keywords internal
.Rec2Advice <- function(Rec, LegacyData = NULL) {
  args <- list()

  if (length(Rec@TAC) && !all(is.na(Rec@TAC))) {
    args$TAC     <- Rec@TAC[1]
    args$TACType <- 'Removals'
  }

  if (length(Rec@Effort) && !all(is.na(Rec@Effort))) {
    args$Effort <- Rec@Effort[1]
    args$EffType <- 'Rel'
  }

  if (length(Rec@Spatial) && !all(is.na(Rec@Spatial)))
    args$Closure <- as.numeric(Rec@Spatial)

  Notes <- character()

  if (.RecSet(Rec@LR5) || .RecSet(Rec@LFR) || .RecSet(Rec@HS)) {
    if (.RecSet(Rec@LR5) != .RecSet(Rec@LFR)) {
      Notes <- c(Notes, "Legacy `Rec@LR5` and `Rec@LFR` must both be set; retention advice ignored.")
    } else {
      Pars <- if (.RecSet(Rec@LR5)) list(LR5 = Rec@LR5[1], LFR = Rec@LFR[1]) else list(LR5 = 0, LFR = 0)
      if (.RecSet(Rec@Rmaxlen)) Pars$Rmaxlen <- Rec@Rmaxlen[1]
      if (.RecSet(Rec@HS)) {
        Pars$HS <- Rec@HS[1]
        args$Retention <- Retention(Pars = Pars, Model = .LegacySlotRetention)
      } else {
        args$Retention <- Retention(Pars = Pars)
      }
    }
  }

  if (.RecSet(Rec@L5) || .RecSet(Rec@LFS)) {
    if (.RecSet(Rec@L5) != .RecSet(Rec@LFS)) {
      Notes <- c(Notes, "Legacy `Rec@L5` and `Rec@LFS` must both be set; selectivity advice ignored.")
    } else {
      Pars <- list(L5 = Rec@L5[1], LFS = Rec@LFS[1])
      if (.RecSet(Rec@Vmaxlen)) Pars$Vmaxlen <- Rec@Vmaxlen[1]
      args$Selectivity <- Selectivity(Pars = Pars)
    }
  }

  if (length(Rec@Misc))
    args$Misc <- Rec@Misc

  Advice <- do.call(Advice, args)

  for (note in Notes)
    Advice <- .CaptureLog(Advice, note, name = ".Rec2Advice", type = "assumption")

  if ((length(Rec@Fdisc) && !all(is.na(Rec@Fdisc))) ||
      (length(Rec@DR) && !all(is.na(Rec@DR))))
    Advice <- .CaptureLog(
      Advice, "Legacy `Rec@Fdisc`/`Rec@DR` (discard mortality) is not currently mapped to `Advice@DiscardMortality`.",
      name = ".Rec2Advice", type = "assumption"
    )

  if (!is.null(LegacyData) && length(LegacyData@Log)) {
    for (type in names(LegacyData@Log))
      Advice@Log[[type]] <- c(Advice@Log[[type]], LegacyData@Log[[type]])
  }

  Advice
}
