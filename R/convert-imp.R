#' @rdname Convert
#' @param Obs An [Imp-class] object#' 
#' @export
ConvertImp <- function(Imp, silent = FALSE) {
  CheckClass(Imp, "Imp", "Imp")
  
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