.CheckRelations <- function(OM) {
  if (length(OM@Relations) > 0)
    cli::cli_abort(c(
      "{.code OM@Relations} is not currently used in simulations.",
      "i" = "Set {.code Relations(OM) <- NULL} to simulate this OM."
    ))
  invisible(OM)
}
