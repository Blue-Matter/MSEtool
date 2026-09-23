#' Summarize Effort/TAC Solver Convergence
#'
#' Reports how often the effort/TAC solver failed to converge within
#' tolerance during a [Project()]/[runMSE()] run, and — where recorded — the
#' per-fleet or per-complex TAC vs. achieved-catch shortfall behind each
#' failure. TAC saturation (TAC unachievable within the effort ceiling, e.g.
#' because the stock is too depleted) and expected multi-complex choke
#' trade-offs are never logged, since they reflect stock dynamics or fleet
#' allocation rather than a solver problem — every row returned here is a
#' genuine solver failure.
#'
#' The `TAC` values reported are the *effective* TAC the solver actually
#' targeted, i.e. after any implementation error (`OM@Imp`) has already been
#' applied — the same target realised landings/removals should be compared
#' against.
#'
#' @param MSE An [mse-class] object.
#' @param silent Logical. Suppress the console summary? Default `FALSE`.
#'
#' @return Invisibly, a `data.frame` with one row per fleet or complex
#'   involved in a logged failure: `Sim`, `Year`, `MP`, `Level` (`"Fleet"` or
#'   `"Complex"`), `Name`, `TAC`, `Catch`, `PctAchieved` (`100 * Catch /
#'   TAC`). Zero rows if nothing was logged. `Level`/`Name`/`TAC`/`Catch`/
#'   `PctAchieved` are `NA` for log entries recorded before this per-
#'   fleet/complex detail was captured.
#'
#' @seealso [Project()], [runMSE()], [MPFailureRate()]
#' @export
SummarizeConvergence <- function(MSE, silent = FALSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  entries <- Filter(\(e) .IsLogEntry(e) && identical(e$name, 'EffortConvergence'),
                    MSE@Log$warning)

  empty <- data.frame(Sim = integer(), Year = numeric(), MP = character(),
                      Level = character(), Name = character(),
                      TAC = numeric(), Catch = numeric(), PctAchieved = numeric())

  if (!length(entries)) {
    if (!silent)
      cli::cli_alert_success("No effort/TAC solver convergence issues logged.")
    return(invisible(empty))
  }

  df <- purrr::map_dfr(entries, \(e) {
    nms <- e$Names
    if (is.null(nms) || !length(nms)) {
      return(data.frame(
        Sim = e$sim %||NA% NA_integer_, Year = e$year %||NA% NA_real_,
        MP  = e$mp %||NA% NA_character_, Level = NA_character_,
        Name = NA_character_, TAC = NA_real_, Catch = NA_real_,
        PctAchieved = NA_real_
      ))
    }
    data.frame(
      Sim   = e$sim %||NA% NA_integer_, Year = e$year %||NA% NA_real_,
      MP    = e$mp %||NA% NA_character_, Level = e$Level %||NA% NA_character_,
      Name  = nms, TAC = e$TAC, Catch = e$Catch,
      PctAchieved = round(100 * e$Catch / e$TAC, 1)
    )
  })

  if (!silent) {
    bySimYear <- unique(df[c('Sim', 'Year', 'MP')])
    byMP <- bySimYear |> dplyr::count(.data$MP)
    cli::cli_alert_warning("{nrow(bySimYear)} effort/TAC solver convergence issue{?s} logged:")
    for (i in seq_len(nrow(byMP)))
      cli::cli_bullets(c('*' = "{byMP$MP[i]}: {byMP$n[i]} did not converge"))
  }

  invisible(df)
}

#' Per-MP Effort/TAC Solver Failure Rate
#'
#' Summarizes [SummarizeConvergence()] into a failure rate per MP, to help
#' decide whether an MP's results are trustworthy enough to keep in the
#' analysis, or whether it failed to converge too often and should be
#' excluded.
#'
#' @param MSE An [mse-class] object.
#'
#' @return A `data.frame` with one row per MP that has projection results:
#'   `MP`, `N_Failed` (number of distinct sim/year combinations with at
#'   least one logged convergence failure), `N_Total` (`nSim(MSE) *
#'   length(Years(MSE, 'Projection'))`), `FailureRate` (`N_Failed /
#'   N_Total`). Sorted descending by `FailureRate`.
#'
#' @seealso [SummarizeConvergence()]
#' @export
MPFailureRate <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  mpNames <- names(PPD(MSE))
  nTotal  <- nSim(MSE) * length(Years(MSE, 'Projection'))

  df <- SummarizeConvergence(MSE, silent = TRUE)
  nFailed <- if (nrow(df)) {
    unique(df[c('Sim', 'Year', 'MP')]) |> dplyr::count(.data$MP, name = 'N_Failed')
  } else {
    data.frame(MP = character(), N_Failed = integer())
  }

  data.frame(MP = mpNames, N_Total = nTotal) |>
    dplyr::left_join(nFailed, by = 'MP') |>
    dplyr::mutate(
      N_Failed    = tidyr::replace_na(.data$N_Failed, 0L),
      FailureRate = .data$N_Failed / .data$N_Total
    ) |>
    dplyr::arrange(dplyr::desc(.data$FailureRate)) |>
    dplyr::relocate('MP', 'N_Failed', 'N_Total', 'FailureRate')
}
