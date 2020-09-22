#' Subset a Stock, Fleet, Obs, or Imp object from an OM object
#' 
#' A function that strips out a Stock, Fleet, Obs, or Imp object from a 
#' complete OM object. Mainly used for internal functions.
#' 
#' @param OM An operating model object (class OM)
#' @param Sub A character string specifying what object type to strip out
#' "Stock", "Fleet", "Obs", or "Imp"
#' @return An object of class Stock, Fleet, Obs, or Imp
#' @author A. Hordyk
#' @examples 
#' Stock <- SubOM(testOM, "Stock")
#' class(Stock)
#' @export 
SubOM <- function(OM, Sub=c("Stock", "Fleet", "Obs", "Imp")) {
  if (class(OM) !="OM") stop("OM must be of class OM ", call.=FALSE)
  Sub <- match.arg(Sub)
  temp <- new(Sub)
  
  slots <- slotNames(temp)
  for (X in seq_along(slots)) 
    slot(temp, slots[X]) <- slot(OM, slots[X]) 
  
  colon <- gregexpr(":", temp@Name)
  space <- gregexpr("  ", temp@Name)
  ind <- switch(Sub, Stock=1, Fleet=2, Obs=3, Imp=4)
  
  if (length(colon) > 0) {
    if (all(colon[[1]] >0) & all(space[[1]]>0)) {
      if (ind < 4) temp@Name <- substr(temp@Name, colon[[1]][ind]+1, space[[1]][ind]-1)
      if (ind == 4) temp@Name <- substr(temp@Name, colon[[1]][ind]+1, nchar(temp@Name))
      
    }
  }
  temp 
}


#' Replace an existing Stock, Fleet, Obs, or Imp object 
#' 
#' A function that replaces a Stock, Fleet, Obs, or Imp object from an 
#' OM with one from another object.
#' 
#' @param OM An operating model object (class OM) which will be updated with a sub-model from another OM
#' @param from An object of class `OM`, `Stock`, `Fleet`, `Obs`, or `Imp` to be replace the values in `OM`
#' @param Sub A character string specifying what object type to replace (only used if `from` is class `OM`)
#' "Stock", "Fleet", "Obs", or "Imp" (default is all four which is probably not what you want to do)
#' @param Name Character. Name for the new OM object (`OM@Name`)
#' @param silent Should messages be printed?
#' 
#' @templateVar url modifying-the-om
#' @templateVar ref the-replace-function
# #' @template userguide_link
#' 
#' @return An object of class OM
#' @author A. Hordyk
#' @examples 
#' # Replace Stock 
#' OM <- DLMtool::testOM
#' OM2 <- Replace(OM, Blue_shark)
#' 
#' # Replace Fleet 
#' OM <- DLMtool::testOM
#' OM2 <- Replace(OM, Generic_DecE)
#' 
#' # Replace Fleet from another OM 
#' OM1 <- new("OM", Albacore, Generic_DecE, Perfect_Info, Overages)
#' OM2 <- new("OM", Blue_shark, Generic_IncE, Generic_Obs, Perfect_Imp)
#' OM1a <- Replace(OM1, OM2, "Fleet")
#' 
#' @export 
Replace <- function(OM, from,Sub=c("Stock", "Fleet", "Obs", "Imp"),  Name=NULL, silent=FALSE) {
  if (class(OM) =="character") OM <- get(OM)
  if (class(OM) !="OM") stop("OM must be of class OM ", call.=FALSE)
  if (class(from) =="character") from <- get(from)
  if (!class(from) %in% c("OM", "Stock", "Fleet", "Obs", "Imp")) 
    stop("from must be class `OM`, `Stock`, `Fleet`, `Obs`, `Imp`", call.=FALSE)
  
  Stock <- SubOM(OM, "Stock")
  Fleet <- SubOM(OM, "Fleet")
  Obs <- SubOM(OM, "Obs")
  Imp <- SubOM(OM, "Imp")
  
  if (class(from) == "OM") {
    Sub <- match.arg(Sub, several.ok=TRUE)
    if (length(Sub)==4) warning("Replacing all OM components. Probably not what you want to do ...")
    
    if(!silent) message("Replacing sub-models:", paste0(" ", Sub))
    for (x in 1:length(Sub)) {
      assign(Sub[x], SubOM(from, Sub[x]))
    }
    
  } else {
    if(!silent) message("Replacing sub-model: ", class(from))
    assign(class(from), from)
  }
  outOM <- new("OM", Stock, Fleet, Obs, Imp) 
  
  
  OMsl <- slotNames('OM')
  allSl <- c(slotNames('Stock'), slotNames('Fleet'), slotNames('Obs'), slotNames('Imp'))
  repsl <- OMsl[!OMsl %in% allSl]
  for (sl in repsl) slot(outOM, sl) <- slot(OM, sl)
  
  if (is.null(Name)) {
    slot(outOM, 'Name') <- paste0('REPLACED -- ', slot(OM, 'Name'))
  } else {
    slot(outOM, 'Name') <- Name  
  }
  
  outOM 
} 
