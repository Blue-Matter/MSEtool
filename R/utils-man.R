
TechManLink <- function() {
  "See the [openMSE Technical Manual](https://docs.openmse.com/) for more details."
}

AdviceArrayInfo <- function(class) {
  
  paste0(
  "
  ## Use in `Advice()`
  
  
  When  [", class, "-class] objects are used in [Advice()] objects, the
  `Sim` and `Year` dimensions should not be included.
  
  * `MeanAtAge` should be either length `nAge` or length `1` (recycled over age classes).
  * `MeanAtLength` should be either length `Classes` or length `1` (recycled over length classes).
  * `MeanAtWeight` (if applicalbe) should be either length `Classes` or length `1` (recycled over weight classes).
  
  If `Classes` is not specified, the classes will be taken from the `Length` or `Weight` object of the 
  corresponding stock.
  "
  )
  
}