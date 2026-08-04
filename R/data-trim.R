
#' Trim Data Object by Final Year
#'
#' Truncate a `Data` object so that all time-dependent arrays are trimmed 
#' to the specified final year. 
#' 
#'
#' @param Data A `data` class object containing year-indexed information.
#' @param Year A single numeric value specifying the final year to retain.
#'
#' Arrays that are not indexed by `Year` are left
#' unchanged. If the supplied year is already the maximum year in the object,
#' the input object is returned unchanged. The `Advice` object is preserved from the
#' original `Data` object.
#' 
#' * `Year` must be numeric and of length one.
#' * `Year` must be contained in `Years(Data)`.
#' 
#' @return
#' A `Data` object where all year-dependent slots include only years less than
#' or equal to `Year`.
#'
#'
#' @seealso
#' * `.SubsetYear()` for year-based subsetting
#' * `Years()` for extracting available years
#'
#' @export
DataTrim <- function(Data, Year) {
  .CheckClass(Data, 'data', 'Data')

  if (!is.numeric(Year))
    cli::cli_abort("`Year` must be a numeric value")
  if (!length(Year)==1)
    cli::cli_abort("`Year` must be a numeric value length 1")
  
  Years <- Data@Years
  if (!Year %in% Years)
    cli::cli_abort("{.var Year} {.val {Year}} is not in `Years(Data)`: {.val {Years(Data)}}")
  
  if (Year == max(Data@Years))
    return(Data)
  
  OutYears <- Years[Years <= Year]
  
  OutData <- .SubsetYear(Data, OutYears, Impute =FALSE)
  # OutData@Advice <-  Data@Advice
  OutData
}
