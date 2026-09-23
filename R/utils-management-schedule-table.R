#' Build a markdown table of the TAC calculation/implementation schedule
#'
#' Shows which projection years are management years (years in which an MP
#' is actually called) and which calendar year of data the `DataLag` rule
#' calls for at each one. See [DataLag()] for a full explanation of what the
#' data lag means and how the data year for a given management year is
#' worked out in practice.
#'
#' @param FirstYear Numeric, or an [om-class], [hist-class], or [mse-class]
#'   object. If numeric, the first implementation year (the first management
#'   year). If an `om` object,
#'   `FirstYear`, `DataLag`, and `Interval` are derived from it (`MPStartYear`
#'   if set, otherwise `CurrentYear + 1`; [DataLag()]; and [Interval()]),
#'   and the corresponding arguments below are ignored.
#' @param DataLag   Numeric. Years of data lag applied at each management year.
#' @param Interval  Numeric. Management update interval, in years.
#' @param nYears    Integer. Number of implementation years to show.
#'   Default shows 3 full management cycles.
#' @param print Logical. Print the data.frame to console with `cat()`?
#'
#' @details
#' The "DataYear used" column follows the `DataLag` rule described in
#' [DataLag()]: for each management year, the data year is one year before
#' it, shifted back by a further `DataLag` years. See [DataLag()] for how
#' this interacts with `MPStartYear` and `InterimAdvice`.
#'
#' @export
#'
#' @seealso [DataLag()] for what the data lag means and how it is applied,
#'   [Interval()] for the management update frequency.
#'
#' @return if `print = TRUE` invisibly returns the underlying data.frame, and
#' `cat()`s a markdown table. If `print == FALSE`, returns the data.frame
ManagementScheduleTable <- function(FirstYear, DataLag, Interval,
                                    nYears = Interval * 2 + 1,
                                    print  = TRUE) {

  if (methods::is(FirstYear, "hist") || methods::is(FirstYear, "mse")) 
    FirstYear <- FirstYear@OM
  
  if (methods::is(FirstYear, "om")) {
    om <- FirstYear
    Interval  <- om@Interval[1]
    DataLag   <- om@DataLag
    FirstYear <- om@MPStartYear
    if (is.null(FirstYear)) FirstYear <- om@CurrentYear + 1
  }

  Years <- FirstYear + seq_len(nYears) - 1
  ManagementYears <- Years[seq(1, length(Years), by = Interval)]
  
  t <- Years - 1
  IsManagement <- Years %in% ManagementYears
  DataYear <- t - DataLag
  
  LastMgmtYear <- vapply(Years, \(y) max(ManagementYears[ManagementYears <= y]), numeric(1))
  
  TACDecision <- ifelse(
    IsManagement,
    sprintf("Calculated in %d, implemented %d", t, Years),
    sprintf("Carries forward %d's TAC", LastMgmtYear)
  )
  
  df <- data.frame(
    Year           = Years,
    t              = t,
    ManagementYear = ifelse(IsManagement, "Yes", "No"),
    TACDecision    = TACDecision,
    DataYearUsed   = ifelse(IsManagement, DataYear, "\u2014")
  )
  names(df) <- c("Implementation Year", "Calculation Year",
                 "Management Year?", "TAC decision", "DataYear used")
  
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows   <- apply(df, 1, \(r) paste0("| ", paste(r, collapse = " | "), " |"))
  
  if (print) {
    cat(c(header, sep, rows), sep = "\n")  
    return(invisible(df))
  }
  df
}
