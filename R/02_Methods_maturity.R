#' @rdname Maturity
setMethod(
  "Maturity",
  signature(Pars = "missing"),
  function(Pars, ...) {
    methods::new("maturity")
  }
)

#' @rdname Maturity
setMethod(
  "Maturity",
  signature(Pars = "list"),
  function(Pars,
           Model = NULL,
           MeanAtAge = NULL,
           MeanAtLength = NULL,
           MeanAtWeight = NULL,
           Classes = NULL,
           Semelparous = FALSE,
           Misc = list(),
           ...) {
    
    methods::new(
      "maturity",
      Pars = Pars,
      Model = Model,
      MeanAtAge = MeanAtAge,
      MeanAtLength = MeanAtLength,
      MeanAtWeight = MeanAtWeight,
      Classes = Classes,
      Semelparous = Semelparous,
      Misc = Misc
    )
  }
)

#' @rdname Maturity
setMethod(
  "Maturity",
  signature(Pars = "stock"),
  function(Pars, ...) {
    Pars@Maturity
  }
)

#' @rdname Maturity
setReplaceMethod(
  "Maturity",
  signature(x = "stock", value = "maturity"),
  function(x, value) {
    slot(x, slot) <- value
    methods::validObject(x)
    x
  }
)


