#' Populate the Age-Size Key for a Length or Weight Object
#'
#' Computes and stores the Age-Length Key (ALK) or Age-Weight Key (AWK) for
#' `object` by calling [CalcAgeSizeKey()] with the object's `MeanAtAge`,
#' `CVatAge`, `Classes`, `TruncSD`, and `Dist` slots. If the object has a
#' `Timing` slot, age class midpoints are shifted by `object@Timing` before
#' the key is computed. The result is stored in `object@ALK` (if
#' `type = "Length"`) or `object@AWK` (if `type = "Weight"`).
#'
#' @param object A [Length()] or [Weight()] object with slots `MeanAtAge`,
#'   `CVatAge`, `Classes` (lower bounds of size bins), `TruncSD`, and `Dist`.
#' @param Ages An [ages-class] object supplying age class values. Required.
#' @param silent Logical. If `TRUE`, suppresses messages from
#'   [CalcAgeSizeKey()]. Default `FALSE`.
#' @param type Character. One of `"Length"` (default) or `"Weight"`,
#'   controlling whether the result is stored in `object@ALK` or `object@AWK`.
#'
#' @return `object` with `object@ALK` or `object@AWK` populated with a named
#'   array of dimensions `Sim × Age × Class × Year`.
#' @keywords internal
.PopulateASK <- function(object, Ages=NULL, silent=FALSE, type='Length') {

  .CheckRequiredObject(Ages, 'ages', 'Ages')

  if ('Timing' %in% slotNames(object))
    Ages@Classes <- Ages@Classes + object@Timing
  
  ASK <- CalcAgeSizeKey(
    MeanAtAge = object@MeanAtAge,
    CVatAge   = object@CVatAge,
    Classes   = object@Classes,
    TruncSD   = object@TruncSD,
    Dist      = object@Dist,
    silent    = silent
  )
  
  if (is.null(dimnames(ASK))) {
    Years <- sort(unique(c(
      dimnames(object@MeanAtAge)[['Year']],
      dimnames(object@CVatAge)[['Year']]
    )))
    dd <- dim(ASK)
    dimnames(ASK) <- list(
      Sim   = seq_len(dd[1]),
      Age   = Ages@Classes,
      Class = object@Classes,
      Year  = Years
    )
  }
  
  if (type == 'Length') {
    object@ALK <- ASK
  } else {
    object@AWK <- ASK
  }
  object
}
