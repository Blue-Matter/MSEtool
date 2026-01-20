#' @rdname Fecundity
setMethod(
  "Fecundity",
  signature(Pars = "missing"),
  function(Pars, ...) {
    methods::new("fecundity")
  }
)

#' @rdname Fecundity
setMethod(
  "Fecundity",
  signature(Pars = "list"),
  function(Pars,
           Model = NULL,
           Units = "eggs",
           MeanAtAge = NULL,
           MeanAtLength = NULL,
           Classes = NULL,
           Misc = list(),
           ...) {
    
    .Object <- methods::new(
      "fecundity",
      Pars = Pars,
      Model = Model,
      Units = Units,
      MeanAtAge = MeanAtAge,
      MeanAtLength = MeanAtLength,
      Classes = Classes,
      Misc = Misc
    )
    
    validObject(.Object)
    .Object
  }
)

#' @rdname Fecundity
setMethod(
  "Fecundity",
  signature(Pars = "stock"),
  function(Pars, ...) {
    Pars@Fecundity
  }
)

#' @rdname Fecundity
setReplaceMethod(
  "Fecundity",
  signature(x = "stock", value = "fecundity"),
  function(x, value) {
    slot(x, slot) <- value
    methods::validObject(x)
    x
  }
)

