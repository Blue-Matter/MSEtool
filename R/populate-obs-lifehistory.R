
# Validation of the life history and exploitation observation specifications.
# Each component is a list with optional `Bias` and `CV`, each a value for all
# parameters or a named list of values for individual parameters.
# `Stock` holds values conditioned on real data, by stock.

.ObsSpecFields <- c("Bias", "CV", "Stock")

.CheckObsSpecValue <- function(x, field, comp, label) {
  if (is.null(x)) return(invisible())
  Vals <- if (is.list(x)) x else .SpecAsList(x)
  Vals <- if (is.list(Vals)) Vals else list(Vals)
  for (v in Vals) {
    if (!is.numeric(v) || anyNA(v) || any(v < 0))
      cli::cli_abort(c(
        "x" = "{.arg {comp}${field}} in {.fn {label}} must contain non-negative numbers.",
        "i" = "Use a number, a vector, or a list of numbers named by parameter."
      ), call = NULL)
  }
  invisible()
}

.CheckObsSpec <- function(Spec, comp, label) {
  if (!length(Spec)) return(Spec)
  if (!is.list(Spec) || is.null(names(Spec)) || any(!nzchar(names(Spec))))
    cli::cli_abort(c(
      "x" = "{.arg {comp}} in {.fn {label}} must be a named list.",
      "i" = "Use {.code list(Bias = , CV = )}; see {.help MSEtool::{label}}."
    ), call = NULL)
  Unknown <- setdiff(names(Spec), .ObsSpecFields)
  if (length(Unknown))
    cli::cli_abort(c(
      "x" = "{.arg {comp}} in {.fn {label}} has unknown element{?s} {.val {Unknown}}.",
      "i" = "Use {.val {c('Bias', 'CV')}}."
    ), call = NULL)
  for (field in c("Bias", "CV"))
    .CheckObsSpecValue(Spec[[field]], field, comp, label)
  Spec
}

.PopulateObsSpecs <- function(Obs, comps, label) {
  for (comp in comps)
    methods::slot(Obs, comp) <- .CheckObsSpec(methods::slot(Obs, comp), comp, label)
  Obs
}

.PopulateLifeHistoryObs <- function(LifeHistory) {
  Unused <- intersect(c("Ages", "Spatial"), methods::slotNames(LifeHistory))
  Unused <- Unused[vapply(Unused, \(s) length(methods::slot(LifeHistory, s)) > 0, logical(1))]
  if (length(Unused))
    cli::cli_warn(
      "{.fn LifeHistoryObs}: observation error for {.field {Unused}} is not used; {cli::qty(length(Unused))}{?it is/they are} observed without error.",
      call = NULL)
  .PopulateObsSpecs(LifeHistory, .LHComponents, "LifeHistoryObs")
}

.PopulateExploitationObs <- function(Exploitation)
  .PopulateObsSpecs(Exploitation, .ExComponents, "ExploitationObs")
