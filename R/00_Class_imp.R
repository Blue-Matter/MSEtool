#' Imp Object
#' @include 00_Class_unions.R
#' 
#' @name ImpClass
#'
#' @export
setClass('imp',
         slots=c(
                 Misc='MiscClass' 
         ))
