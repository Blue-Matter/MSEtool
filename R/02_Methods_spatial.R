#' @rdname Spatial
#' @export
setMethod(
  "Spatial",
  signature(UnfishedDist = "missing"),
  function(UnfishedDist, ...) {
    methods::new("spatial")
  }
)


#' @rdname Spatial
#' @export
setMethod(
  "Spatial",
  signature(UnfishedDist = "ANY"),
  function(UnfishedDist,
           ProbStaying  = NULL,
           RelativeSize = NULL,
           Movement     = NULL,
           FracOther    = NULL,
           Arrangement = NULL,
           CVDist       = 0.1,
           CVStay       = 1,
           Misc         = list()) {
    
    methods::new(
      "spatial",
      UnfishedDist = UnfishedDist,
      ProbStaying  = ProbStaying,
      RelativeSize = RelativeSize,
      Movement     = Movement,
      FracOther    = FracOther,
      Arrangement = Arrangement,
      CVDist       = CVDist,
      CVStay       = CVStay,
      Misc         = Misc
    )
  }
)

#' @rdname Spatial
#' @include 00_Class_stock.R
#' @export
setMethod(
  "Spatial",
  signature(UnfishedDist = "stock"),
  function(UnfishedDist, ...) {
    UnfishedDist@Spatial
  }
)

#' @rdname Spatial
#' @include 00_Class_advice.R
#' @export
setMethod(
  "Spatial",
  signature(UnfishedDist = "advice"),
  function(UnfishedDist, ...) {
    UnfishedDist@Spatial
  }
)

#' @rdname Spatial
#' @include 00_Class_stock.R
#' @export
setReplaceMethod(
  "Spatial",
  signature(x = "stock", value = "spatial"),
  function(x, value) {
    x@Spatial <- value
    x
  }
)

#' @rdname Spatial
#' @include 00_Class_advice.R
#' @export
setReplaceMethod(
  "Spatial",
  signature(x = "advice", value = "spatial"),
  function(x, value) {
    x@Spatial <- value
    x
  }
)


