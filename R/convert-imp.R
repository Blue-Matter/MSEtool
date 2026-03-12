#' Convert a Legacy Imp Object to a New imp Class
#'
#' Converts a legacy [Imp-legacy-class] object to the current [imp-class] by
#' mapping implementation error parameters to their corresponding new S4 slots.
#'
#' @param Imp An [Imp-legacy-class] object to convert.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @return An [imp-class] object with `TAC`, `Effort`, and `Size`
#'   implementation error slots populated.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [ConvertObs()]
#'
#' @examples
#' \dontrun{
#' Implegacy <- readRDS("MyLegacyImp.rds")
#' imp_new <- ConvertImp(Implegacy)
#' }
#'
#' @export
ConvertImp <- function(Imp, silent = FALSE) {
  CheckClass(Imp, c("Imp", 'OM'), "Imp")
  
  if (!silent) {
    cli::cli_alert("Converting object of class {.cls Imp} to class {.cls imp}")
  }
  
  
  imp <- Imp()
  imp@Name <- Imp@Name
  
  imp@TAC@Mean <- Imp@TACFrac
  imp@TAC@SD <- Imp@TACSD
  
  imp@Effort@Mean <- Imp@TAEFrac
  imp@Effort@SD <- Imp@TAESD
  
  imp@Size@Mean <- Imp@SizeLimFrac
  imp@Size@SD <- Imp@SizeLimSD
  
  imp
}