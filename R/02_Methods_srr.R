#' @rdname SRR
setMethod(
  "SRR",
  signature(Pars = "missing"),
  function(Pars, ...) {
    methods::new("srr")
  }
)


#' @rdname SRR
setMethod(
  "SRR",
  signature(Pars = "list"),
  function(Pars,
           Model = "BevertonHolt",
           R0 = array(),
           SD = array(),
           AC = array(),
           SPFrom = NULL,
           TruncSD = 2,
           RecDevInit = array(),
           RecDevHist = array(),
           RecDevProj = array(),
           SpawnTimeFrac = 0,
           RelRecFun = NULL,
           Units = 1,
           Misc = list(),
           ...) {
    
    methods::new(
      "srr",
      Pars = Pars,
      Model = Model,
      R0 = R0,
      SD = SD,
      AC = AC,
      SPFrom = SPFrom,
      TruncSD = TruncSD,
      RecDevInit = RecDevInit,
      RecDevHist = RecDevHist,
      RecDevProj = RecDevProj,
      SpawnTimeFrac = SpawnTimeFrac,
      RelRecFun = RelRecFun,
      Units = Units,
      Misc = Misc
    )
  }
)

#' @rdname SRR
setMethod(
  "SRR",
  signature(Pars = "stock"),
  function(Pars, ...) {
    Pars@SRR
  }
)

#' @rdname SRR
setReplaceMethod(
  "SRR",
  signature(x = "stock", value = "srr"),
  function(x, value) {
    x@SRR <- value
    x
  }
)


