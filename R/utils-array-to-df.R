#' Convert Between Arrays and Data Frames
#'
#' `Array2DF()` converts a matrix or array to a tidy data frame, with dimension
#' names mapped to columns (`Sim`, `Stock`, `Age`, `Year`, `Fleet`, `Area`) and
#' values in a `Value` column. Numeric columns are coerced with [as.numeric()];
#' `Stock` and `Fleet` are returned as ordered factors.
#'
#' `DF2Array()` is the inverse: it reshapes a tidy data frame back into a named
#' array, using any columns whose names match `Sim`, `Stock`, `Age`, `Year`,
#' `Fleet`, or `Area` as array dimensions.
#'
#' @param array A `matrix` or `array` object.
#' @param DF A `data.frame` containing a `Value` column and at least one
#'   dimension column (`Sim`, `Stock`, `Age`, `Year`, `Fleet`, `Area`).
#'
#' @return
#' * `Array2DF()` returns a `data.frame` with one row per array
#'   element and one column per dimension plus a `Value` column.
#' * `DF2Array()` returns a named `array` whose dimensions and dimnames are
#'   derived from the unique values of the dimension columns in `DF`.
#'   
#' @examples
#' # Round-trip: array -> data frame -> array
#' a <- array(1:24, dim = c(2, 3, 4),
#'            dimnames = list(Sim = 1:2, Year = 1:3, Age = 1:4))
#' df <- Array2DF(a)
#' df
#' DF2Array(df)
#'
#' @name array_df_conversion
NULL

#' @rdname array_df_conversion
#' @export
Array2DF <- function(array) {
  if (!inherits(array, c('matrix', 'array'))) 
    cli::cli_abort('`array` in not class `matrix` or `array`')
  
  array2DF(array) |> ConvertDF()
}

#' @rdname array_df_conversion
#' @export
DF2Array <- function(DF) {
  if (!inherits(DF, 'data.frame'))
    cli::cli_abort("`DF` is not a data.frame")
  
  nms <- names(DF)
  PosNames <- c("Sim", "Stock", "Age", "Year", "Fleet", "Area") 
  DFNames <- nms[nms %in% PosNames]
  PosNames <- PosNames[PosNames %in% DFNames]
  
  df <- DF |> 
    dplyr::select(dplyr::all_of(DFNames), 'Value') |>
    dplyr::relocate(dplyr::all_of(PosNames)) 
  
  temp <- df
  temp$Value <- NULL
  DimNames <- apply(temp, 2, unique, simplify = FALSE)
  Dim <- lapply(DimNames, length)
  array(df$Value, dim=Dim, dimnames=DimNames)
}

MakeFactor <- function(x) {
  factor(x, ordered = TRUE, levels=unique(x))
}

ArrangeDF <- function(df) {
  
  cnames <- colnames(df)
  colInd <- c('Sim', 'Year', 'Age') %in% cnames
  
  if (prod(colInd))
    return(
      df |> dplyr::arrange(Sim, Year, Age)
    )
  
  if (prod(colInd[1:2]))
    return(
      df |> dplyr::arrange(Sim, Year)
    )
  
  if (prod(colInd[c(1,3)]))
    return(
      df |> dplyr::arrange(Sim, Age)
    )
  
  if (prod(colInd[c(2,3)]))
    return(
      df |> dplyr::arrange(Year, Age)
    )
  
  df
}

ConvertDF <- function(df) {
  nms <- colnames(df)
  if ('Sim'   %in% nms) df$Sim   <- as.numeric(df$Sim)
  if ('Age'   %in% nms) df$Age   <- as.numeric(df$Age)
  if ('Class' %in% nms) df$Class <- as.numeric(df$Class)
  if ('Stock' %in% nms) df$Stock <- MakeFactor(df$Stock)
  if ('Fleet' %in% nms) df$Fleet <- MakeFactor(df$Fleet)
  if ('Year'  %in% nms) df$Year  <- as.numeric(df$Year)
  if ('Area'  %in% nms) df$Area  <- as.numeric(df$Area)
  if ('F'   %in% nms)   df$F   <- as.numeric(df$F)
  if ('Value' %in% nms) {
    chk <- suppressWarnings(as.numeric(df$Value))
    if (!all(is.na(chk))) df$Value <- chk
  }
  
  if (requireNamespace('tibble', quietly = TRUE)) {
    return(tibble::as_tibble(df))
  }
  df
}
