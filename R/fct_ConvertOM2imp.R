OM2imp <- function(OM, cpars=NULL) {
  Imp <- SubOM(OM, 'Imp')
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



