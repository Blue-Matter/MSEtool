#' Condition an Age-Size Key on Size-Selectivity
#'
#' Reweights an Age-Size Key (ALK or AWK) by a size-selectivity vector within
#' each age class, producing the length distribution of fish that are actually
#' caught rather than the population length distribution. Under
#' `sel_mode = "age"` the key is returned unchanged, reflecting the assumption
#' that selectivity acts on age and the within-age size distribution is
#' irrelevant to catch composition.
#'
#' @param key Array with dimensions `Sim, Age, Class, Year`. The population
#'   Age-Size Key where rows (age classes) sum to 1 over `Class`.
#' @param selectivity Array with dimensions `Sim, Class, Year, Area`.
#'   Size-selectivity values in \[0, 1\].
#' @param sel_mode Character. One of `"length"` or `"age"`. If `"length"`,
#'   the key is conditioned on `selectivity`. If `"age"`, `key` is returned
#'   unchanged.
#'
#' @return Array with dimensions `Sim, Age, Class, Year, Area`. Each row
#'   (age class) sums to 1 over `Class`, representing the size distribution
#'   of caught fish of that age.
#'
#' @seealso `.CalcCatchAtSize()`
#' @keywords internal
.ConditionAgeSizeKey <- function(key, selectivity, sel_mode = c("length", "age")) {
  sel_mode <- match.arg(sel_mode)
  
  if (sel_mode == "age" || is.null(selectivity)) return(key)
  
  key_area  <- AddDimension(key,         'Area')
  sel_area  <- AddDimension(selectivity, 'Age', pos = 2)
  weighted  <- ArrayMultiply(key_area, sel_area)
  
  size_classes <- as.numeric(dimnames(sel_area)$Class)
  
  denom <- SumOverClass(weighted)
  denom <- AddDimension(denom, 'Class', pos = 3)
  denom[denom == 0] <- .Machine$double.eps
  denom <- ExtendClasses(denom, Classes = size_classes)
  
  ArrayDivide(weighted, denom)
}
