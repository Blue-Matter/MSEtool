#' @rdname Weight
setMethod("Weight", signature(Pars = "missing"),
          function(Pars, ...) {
            methods::new("weight")
          }
)


#' @rdname Weight
setMethod("Weight", signature(Pars = "list"),
          function(Pars,
                   Model = NULL,
                   Units = "g",
                   MeanAtAge = NULL,
                   MeanAtLength = NULL,
                   CVatAge = NULL,
                   Dist = "lognormal",
                   TruncSD = 2,
                   Timing = 0,
                   Random = NULL,
                   ASK = NULL,
                   Classes = NULL,
                   Misc = list(),
                   ...) {
            
            .Object <- methods::new(
              "weight",
              Pars = Pars,
              Model = Model,
              Units = Units,
              MeanAtAge = MeanAtAge,
              MeanAtLength = MeanAtLength,
              CVatAge = CVatAge,
              Dist = Dist,
              TruncSD = TruncSD,
              Timing = Timing,
              Random = Random,
              ASK = ASK,
              Classes = Classes,
              Misc = Misc
            )
            
            .Object
          }
)

#' @rdname Weight
setMethod("Weight", signature(Pars = "stock"),
          function(Pars, ...) {
            Pars@Weight
          }
)

#' @rdname Weight
setReplaceMethod(
  "Weight",
  signature(x = "stock", value = "weight"),
  function(x, value) {
    assignSlot(x, value, "Weight")
  }
)


