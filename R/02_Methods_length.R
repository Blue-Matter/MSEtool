#' @rdname Length
setMethod("Length", signature(Pars = "missing"), function(Pars, ...) {
    methods::new("length")
  }
)

#' @rdname Length
setMethod(
  "Length", signature(Pars = "list"),
  function(Pars,
           Model = NULL,
           Units = "mm",
           MeanAtAge = NULL,
           CVatAge = 0.1,
           Dist = "normal",
           TruncSD = 2,
           Timing = 0,
           Random = NULL,
           ASK = NULL,
           Classes = NULL,
           Misc = list(),
           ...) {
    
    obj <- methods::new("length",
                        Pars = Pars,
                        Model = Model,
                        Units = Units,
                        MeanAtAge = MeanAtAge,
                        CVatAge = CVatAge,
                        Dist = Dist,
                        TruncSD = TruncSD,
                        Timing = Timing,
                        Random = Random,
                        ASK = ASK,
                        Classes = Classes,
                        Misc = Misc)
    
    validObject(obj)
    obj
  }
)

#' @rdname Length
setMethod(
  "Length", signature(Pars = "stock"),
  function(Pars, ...) {
    Pars@Length
  }
)

#' @rdname Length
setReplaceMethod(
  "Length",
  signature(x = "stock", value = "length"),
  function(x, value) {
    slot(x, slot) <- value
    methods::validObject(x)
    x
  }
)

