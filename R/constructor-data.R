#' Constructor and Accessors for `Data`
#'
#' Internal `Get*` and `Set*` functions used to access and modify slots of a
#' [Data()] object. These functions are not intended for direct user use.
#' User-facing access is provided via accessor and assignment functions 
#'
#' @param object A [Data()] object.
#' @param x An [OM()] or [Hist()] object
#' @param value Value to assign.
#'
#' @return
#' * `Get*` functions return the slot value.
#' * `Set*` functions return a modified [Data()] object.
#'
#' @export
Data <- function(x) {
  if (methods::is(x, "om")) {
    return(x@Data)
  }
  
  if (methods::is(x, "hist")) {
    return(x@Data)
  }
  
  methods::new("data")
}
