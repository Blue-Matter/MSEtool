#' @include 00_Class_unions.R
#' @include 00_Class_ages.R
#' @include 00_Class_length.R
#' @include 00_Class_weight.R
#' @include 00_Class_naturalmortality.R
#' @include 00_Class_maturity.R
#' @include 00_Class_fecundity.R 
#' @include 00_Class_srr.R
#' @include 00_Class_spatial.R
#' @include 00_Class_depletion.R
#' @rdname Stock
#' @export
setMethod(
  "Stock",
  signature(x = "missing"),
  function(x, ...) {
    methods::new("stock")
  }
)

#' @rdname Stock
#' @export
setMethod(
  "Stock",
  signature(x = "character"),
  function(x,
           CommonName = NULL,
           Species = NULL,
           Ages = new("ages"),
           Length = new("length"),
           Weight = new("weight"),
           NaturalMortality = new("naturalmortality"),
           Maturity = new("maturity"),
           Fecundity = new("fecundity"),
           SRR = new("srr"),
           Spatial = new("spatial"),
           Depletion = new("depletion"),
           Seasons = 1,
           Misc = list(),
           ...) {
    
    nYear <- 20
    pYear <- 30
    nSim <- 48
    CurrentYear <- as.numeric(format(Sys.Date(), "%Y"))
    
    methods::new(
      "stock",
      Name = x,
      CommonName = CommonName,
      Species = Species,
      Ages = Ages,
      Length = Length,
      Weight = Weight,
      NaturalMortality = NaturalMortality,
      Maturity = Maturity,
      Fecundity = Fecundity,
      SRR = SRR,
      Spatial = Spatial,
      Depletion = Depletion,
      nYear = nYear,
      pYear = pYear,
      nSim = nSim,
      CurrentYear = CurrentYear,
      Seasons = Seasons,
      Years = CalcYears(nYear, pYear, CurrentYear, Seasons),
      Misc = Misc
    )
  }
)


#' @rdname Stock
#' @export
setMethod(
  "Stock",
  signature(x = "om"),
  function(x, i = NULL, ...) {
    if (is.null(i))
      return(x@Stock)
    
    if (!is.numeric(i) || i > length(x@Stock))
      cli::cli_abort("Invalid stock index")
    
    x@Stock[[i]]
  }
)

#' @rdname Stock
#' @export
setReplaceMethod(
  "Stock",
  signature(x = "om", value = "stock"),
  function(x, value) {
    
    x@Stock <- list(value)
    names(x@Stock) <- value@Name
    class(x@Stock) <- "StockList"
    
    x
  }
)

