#' @rdname Depletion
#' @export
setMethod(
  "Depletion",
  signature(x = "missing"),
  function(x, ...) {
    methods::new("depletion")
  }
)


#' @rdname Depletion
#' @export
setMethod(
  "Depletion",
  signature(x = "numeric"),
  function(x,
           Final = numeric(),
           Reference = "B0",
           ...) {
    methods::new(
      "depletion",
      Initial   = x,
      Final     = Final,
      Reference = Reference
    )
  }
)

#' @rdname Depletion
#' @export
setMethod(
  "Depletion",
  signature(x = "stock"),
  function(x, ...) {
    x@Depletion
  }
)

#' @rdname Depletion
#' @export
setReplaceMethod(
  "Depletion",
  signature(x = "stock", value = "depletion"),
  function(x, value) {
    x@Depletion <- value
    x
  }
)
