#' Set Argument Values of a Management Procedure
#'
#' Returns a copy of a management procedure (MP) with new default values for
#' some of its arguments, e.g. to define a variant of a built-in MP with
#' different settings. 
#'
#' @param MP An MP function (class `mp` or `mmp`), or the name of one.
#' @param ... Named argument values. Each name must be an argument of `MP`
#'   (unless `MP` has a `...` argument). A value of `NULL` sets that
#'   argument's default to `NULL`.
#'
#' @return A function of the same class as `MP`.
#'
#' @examples
#' IR5 <- SetMPArgs(IndexRate, CalibYears = 5, Smooth = FALSE)
#' formals(IR5)$CalibYears
#'
#' \dontrun{
#' MSE <- Project(Hist, MPs = list(IR5 = IR5, IR2 = IndexRate))
#' }
#'
#' @seealso [Project()], [TuneMP()]
#' @export
SetMPArgs <- function(MP, ...) {
  if (is.character(MP)) {
    if (length(MP) != 1)
      cli::cli_abort("{.arg MP} must be a single MP function or name.")
    MP <- get(MP, mode = 'function')
  }
  if (!is.function(MP))
    cli::cli_abort("{.arg MP} must be an MP function or the name of one.")

  Args <- list(...)
  if (!length(Args))
    return(MP)

  ArgNames <- names(Args)
  if (is.null(ArgNames) || any(!nzchar(ArgNames)))
    cli::cli_abort("All arguments passed to {.fn SetMPArgs} must be named.")
  if (anyDuplicated(ArgNames))
    cli::cli_abort("Duplicated argument{?s}: {.val {unique(ArgNames[duplicated(ArgNames)])}}.")

  Formals <- as.list(formals(MP))
  if (!'...' %in% names(Formals)) {
    Bad <- setdiff(ArgNames, names(Formals))
    if (length(Bad))
      cli::cli_abort(c(
        "{.val {Bad}} {?is not an argument/are not arguments} of {.arg MP}.",
        "i" = "Arguments: {.val {setdiff(names(Formals), 'Data')}}."
      ))
  }

  Attributes <- attributes(MP)
  for (nm in ArgNames)
    Formals[nm] <- list(Args[[nm]])
  formals(MP) <- Formals

  for (a in setdiff(names(Attributes), 'srcref'))
    attr(MP, a) <- Attributes[[a]]
  MP
}
