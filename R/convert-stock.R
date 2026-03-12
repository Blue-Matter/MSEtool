
#' Convert a Legacy Stock Object to a New stock Class
#'
#' Converts a legacy [Stock-legacy-class] object to the current [stock-class]
#' by extracting and mapping each component to its corresponding new S4 class.
#'
#' @param Stock A [Stock-legacy-class] object to convert.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @return A [stock-class] object.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()]
#'
#' @examples
#' \dontrun{
#' Stocklegacy <- readRDS("MyLegacyStock.rds")
#' stock_new <- ConvertStock(Stocklegacy)
#' }
#'
#' @export
ConvertStock <- function(Stock, silent = FALSE) {
  CheckClass(Stock, "Stock", "Stock")

  if (!silent) {
    cli::cli_alert("Converting object of class {.cls Stock} to class {.cls stock}")
  }

  stock <- Stock2Name(Stock)
  stock@Ages <- Stock2Ages(Stock)
  stock@Length <- Stock2Length(Stock)
  stock@Weight <- Stock2Weight(Stock)
  stock@NaturalMortality <- Stock2NaturalMortality(Stock)
  stock@Maturity <- Stock2Maturity(Stock)
  stock@Fecundity <- Stock2Fecundity(Stock)
  stock@SRR <- Stock2SRR(Stock)
  stock@Spatial <- Stock2Spatial(Stock)
  stock@Depletion <- Stock2Depletion(Stock)
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

Stock2Ages <- function(Stock) {
  Ages(
    MaxAge = Stock@maxage,
    MinAge = 0,
    Units = "year"
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
