#' Summarize Effort/TAC Solver Convergence
#'
#' Reports how often the effort/TAC solver failed to converge within
#' tolerance during a [Project()]/[runMSE()] run. TAC saturation (TAC
#' unachievable within the effort ceiling, e.g. because the stock is too
#' depleted) is not logged, since it reflects the stock dynamics rather than
#' a solver failure.
#' @param MSE An [mse-class] object.
#' @param silent Logical. Suppress the console summary? Default `FALSE`.
#'
#' @return Invisibly, a `data.frame` with one row per logged event: `Sim`,
#'   `Year`, `MP`. Zero rows if nothing was logged.
#'
#' @seealso [Project()], [runMSE()]
#' @export
SummarizeConvergence <- function(MSE, silent = FALSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  entries <- Filter(\(e) .IsLogEntry(e) && identical(e$name, 'EffortConvergence'),
                    MSE@Log$warning)

  if (!length(entries)) {
    if (!silent)
      cli::cli_alert_success("No effort/TAC solver convergence issues logged.")
    return(invisible(data.frame(Sim = integer(), Year = numeric(),
                                MP = character())))
  }

  df <- purrr::map_dfr(entries, \(e) data.frame(
    Sim  = e$sim %||% NA_integer_,
    Year = e$year %||% NA_real_,
    MP   = e$mp %||% NA_character_
  ))

  if (!silent) {
    byMP <- df |> dplyr::count(.data$MP)
    cli::cli_alert_warning("{nrow(df)} effort/TAC solver convergence issue{?s} logged:")
    for (i in seq_len(nrow(byMP)))
      cli::cli_bullets(c('*' = "{byMP$MP[i]}: {byMP$n[i]} did not converge"))
  }

  invisible(df)
}
