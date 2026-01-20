#' @rdname NaturalMortality
setMethod(
  "NaturalMortality",
  signature(Pars = "missing"),
  function(Pars, ...) {
    methods::new("naturalmortality")
  }
)


#' @rdname NaturalMortality
setMethod(
  "NaturalMortality",
  signature(Pars = "list"),
  function(Pars,
           Model = NULL,
           Units = "year",
           MeanAtAge = NULL,
           MeanAtLength = NULL,
           Random = NULL,
           Classes = NULL,
           Misc = list(),
           ...) {
    
    methods::new(
      "naturalmortality",
      Pars = Pars,
      Model = Model,
      Units = Units,
      MeanAtAge = MeanAtAge,
      MeanAtLength = MeanAtLength,
      Random = Random,
      Classes = Classes,
      Misc = Misc
    )
  }
)

#' @rdname NaturalMortality
setMethod(
  "NaturalMortality",
  signature(Pars = "stock"),
  function(Pars, ...) {
    Pars@NaturalMortality
  }
)


#' @rdname NaturalMortality
setReplaceMethod(
  "NaturalMortality",
  signature(x = "stock", value = "naturalmortality"),
  function(x, value) {
    slot(x, slot) <- value
    methods::validObject(x)
    x
  }
)


#' @rdname NaturalMortality
#' @export
NMortality <- function(...) {
  NaturalMortality(...)
}

#' @rdname NaturalMortality
#' @export
`NMortality<-` <- function(x, value) {
  NaturalMortality(x) <- value
  x
}


