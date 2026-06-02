#' Convert a Legacy Stock Object to the Current `stock` Class
#'
#' Converts a legacy [Stock-legacy-class] object to the current [stock-class]
#' by mapping each component to its corresponding new S4 sub-object.
#'
#' @param Stock A [Stock-legacy-class] object to convert. An [OM-legacy-class]
#'   object with a legacy `Stock` slot is also accepted, in which case the
#'   `Stock` slot is extracted and converted.
#' @param Seasons Numeric. Number of seasons in a year. Default `1`.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' ## Slot Mapping
#'
#' The following legacy slots are mapped to their current equivalents:
#'
#' | Legacy slot | New location |
#' |---|---|
#' | `Name`, `Common_Name`, `Species` | `stock@Name`, `@CommonName`, `@Species` |
#' | `maxage` | `Ages@MaxAge` (via [Ages()], `MinAge = 0`) |
#' | `Linf`, `K`, `t0`, `Linfsd`, `Ksd`, `LenCV` | `Length@Pars`, `@CVatAge` |
#' | `a`, `b` | `Weight@Pars$alpha`, `@Pars$beta` |
#' | `M`, `Msd` | `NaturalMortality@Pars` |
#' | `L50`, `L50_95` | `Maturity@Pars`, `@Model = MaturityAtLength` |
#' | `ageM`, `age95` | `Maturity@Pars`, `@Model = MaturityAtAge` (used only when `L50` is absent) |
#' | `h` | `SRR@Pars$h` |
#' | `R0` | `SRR@R0` |
#' | `Perr` | `SRR@SD` |
#' | `AC` | `SRR@AC` |
#' | `SRrel` | `SRR@Model` (`1` → `"BevertonHolt"`, `2` → `"Ricker"`) |
#' | `Size_area_1` | `Spatial@RelativeSize` |
#' | `Prob_staying` | `Spatial@ProbStaying` |
#' | `Frac_area_1` | `Spatial@UnfishedDist` |
#' | `D` | `Depletion@Final` |
#'
#' ## Maturity Priority
#'
#' When both length-based (`L50`, `L50_95`) and age-based (`ageM`, `age95`)
#' maturity parameters are present, the length-based parameters take priority
#' and `Model` is set to [MaturityAtLength]. Age-based parameters are used
#' only when `L50` is absent or `NULL`.
#'
#' ## Spatial Structure
#'
#' If all three spatial parameters (`Size_area_1`, `Prob_staying`,
#' `Frac_area_1`) equal `0.5`, an empty [spatial-class] object is returned.
#' This is treated by the model as a single well-mixed area with no spatial
#' structure.
#'
#'
#' @return A [stock-class] object.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [ConvertFleet()],
#'   [ConvertObs()], [ConvertImp()]
#'
#' @examples
#' \dontrun{
#' stock_legacy <- readRDS("MyLegacyStock.rds")
#' stock_new <- ConvertStock(stock_legacy)
#' }
#'
#' @export
ConvertStock <- function(Stock, Seasons = 1, silent = FALSE) {
  CheckClass(Stock, c("Stock", "OM"), "Stock")
  
  if (!silent)
    cli::cli_alert("Converting object of class {.cls Stock} to class {.cls stock}")
  
  stock                  <- Stock2Name(Stock)
  stock@Ages             <- Stock2Ages(Stock)
  stock@Length           <- Stock2Length(Stock)
  stock@Weight           <- Stock2Weight(Stock)
  stock@NaturalMortality <- Stock2NaturalMortality(Stock)
  stock@Maturity         <- Stock2Maturity(Stock)
  stock@Fecundity        <- Stock2Fecundity(Stock)
  stock@SRR              <- Stock2SRR(Stock)
  stock@Spatial          <- Stock2Spatial(Stock)
  stock@Depletion        <- Stock2Depletion(Stock)
  stock
}

Stock2Name <- function(Stock) {
  stock <- Stock()
  if (inherits(Stock, "OM")) {
    stock@Name <- SubOM(Stock, "Stock")@Name
  } else {
    stock@Name <- Stock@Name
  }
  stock@Name <- gsub("REPLACED -- ", "", stock@Name)
  stock@CommonName <- Stock@Common_Name
  stock@Species <- Stock@Species
  stock
}

Stock2Ages <- function(Stock, Seasons = 1) {
  Ages(
    MaxAge = Stock@maxage,
    MinAge = 0,
    Units = CalcTSUnits(Seasons)
  )
}

Stock2Length <- function(Stock) {
  Length <- Length()
  Length@Pars$Linf <- Stock@Linf
  Length@Pars$K <- Stock@K
  Length@Pars$t0 <- Stock@t0
  Length@Pars$Linfsd <- Stock@Linfsd
  Length@Pars$Ksd <- Stock@Ksd
  
  Length@CVatAge <- Stock@LenCV
  Length@Units <- ""
  Length
}

Stock2Weight <- function(Stock) {
  Weight <- Weight(Pars=list())
  Weight@Pars$alpha <- Stock@a
  Weight@Pars$beta <- Stock@b
  Weight@Units <- ""
  Weight
}

Stock2NaturalMortality <- function(Stock) {
  NaturalMortality <- NaturalMortality()
  NaturalMortality@Pars$M <- Stock@M
  NaturalMortality@Pars$Msd <- Stock@Msd
  NaturalMortality
}

Stock2Maturity <- function(Stock) {
  Maturity <- Maturity()
  Maturity@Pars$L50 <- Stock@L50
  Maturity@Pars$L50_95 <- Stock@L50_95
  Maturity
}

Stock2Fecundity <- function(Stock) {
  Fecundity()
}

switchSRR <- function(SRrel) {
  if (is.null(SRrel))
    return(NULL)
  switch(SRrel,
         '1'='BevertonHolt',
         '2'='Ricker')
}

Stock2SRR <- function(Stock) {
  SRR <- SRR()
  SRR@Pars$h <- Stock@h
  SRR@R0 <- Stock@R0
  SRR@SD <- Stock@Perr
  SRR@AC <- Stock@AC
  SRR@Model <- switchSRR(Stock@SRrel[1])
  if (SRR@Model == "Ricker") {
    SRR@Pars$hR <- SRR@Pars$h
    SRR@Pars$h <- NULL 
  }
  
  SRR
}

Stock2Spatial <- function(Stock) {
  Spatial <- Spatial()
  Spatial@RelativeSize <- Stock@Size_area_1
  Spatial@ProbStaying <- Stock@Prob_staying
  Spatial@UnfishedDist <- Stock@Frac_area_1

  if (all(Spatial@RelativeSize == 0.5) &
    all(Spatial@ProbStaying == 0.5) &
    all(Spatial@UnfishedDist == 0.5)) {
    return(Spatial())
  }
  Spatial
}

Stock2Depletion <- function(Stock) {
  Depletion <- Depletion()
  Depletion@Final <- Stock@D
  Depletion
}
