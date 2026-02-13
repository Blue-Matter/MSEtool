#' Calculate an Age-Size Key
#'
#' Generates an Age-Size key given mean and standard deviation of size-at-age. The size-at-age
#' can be normally or log-normally distributed. By default the distribution is truncated at 2
#' standard deviations.
#'
#' @param MeanAtAge Numeric vector, matrix, or 3D array of mean size-at-age, or
#'  a [Length()] object.
#' @param CVatAge Numeric vector, matrix, or 3D array of coefficient of variation (CV)
#'   at age. Same structure as `MeanAtAge`.
#' @param Classes Numeric vector of midpoints of size classes for the age-size key.
#' @param TruncSD Numeric value for the number of standard deviations at which the
#'   distribution is truncated.
#' @param Dist Character string, either `"normal"` or `"lognormal"` indicating
#'   the distribution of size-at-age.
#' @param AgeClasses Optional numeric vector of age classes. Defaults to `0:(nage-1)`.
#' @param silent Logical; if `TRUE`, suppress progress bar.
#' @param type Character; currently only `"Length"` is supported.
#'
#' @return A 4D array with dimensions `Sim`, `Age`, `Class`, and `Year`.
#'
#' @example man-examples/calc-age-size-key.R
#' @export
CalcAgeSizeKey <- function(MeanAtAge,
                           CVatAge,
                           Classes,
                           TruncSD = 2,
                           Dist = c("normal", "lognormal"),
                           silent = FALSE,
                           type = "Length") {
  
  Dist <- match.arg(Dist, c("normal", "lognormal"))

  if (inherits(MeanAtAge, "length")) {
    LengthObject <- MeanAtAge
    MeanAtAge <- LengthObject@MeanAtAge
    CVatAge <- LengthObject@CVatAge
    Classes <- LengthObject@Classes
    TruncSD <- LengthObject@TruncSD
    Dist <- LengthObject@Dist
  }
  
  # Checks
  if (any(Classes < 0)) {
    cli::cli_abort("Some `Classes` < 0 ")
  }
  if (length(Classes) < 3) {
    cli::cli_abort("`length(Classes)<3`")
  }
  if (length(TruncSD) > 1) {
    TruncSD <- TruncSD[1]
  }
  
  if (TruncSD < 0) {
    cli::cli_abort("`TruncSD` < 0 ")
  }

  MeanAtAge <- Structure(MeanAtAge)
  SDatAge <- ArrayMultiply(MeanAtAge, CVatAge)

  dim_MeanAtAge <- dim(MeanAtAge)
  nage <- dim_MeanAtAge[2]
  dim_SDatAge <- dim(SDatAge)

  if (dim_MeanAtAge[2] != dim_SDatAge[2]) {
    if (dim_SDatAge[2] == 1) {
      SDatAge <- Structure(replicate(nage, SDatAge))
    } else {
      cli::cli_abort("`dim(MeanAtAge)[2] != dim(SDatAge)[2]`")
    }
  }
  
  if (length(dim_MeanAtAge[1])==1 && length( dim_SDatAge[1])==1) {
    nSim <- 1
    sims <- as.numeric(max(dim_MeanAtAge[1], dim_SDatAge[1]))

  } else {
    nSim <- max(dim_MeanAtAge[1], dim_SDatAge[1]) # maximum number of simulations
    sims <- 1:nSim
  }

  YearsList <- list(
    dimnames(MeanAtAge)[["Year"]],
    dimnames(SDatAge)[["Year"]]
  )

  ind <- unlist(lapply(YearsList, length)) |> which.max()
  Years <- YearsList[[ind]]

  if (!is.null(Years)) {
    MeanAtAge <- MeanAtAge |> ExtendSims(nSim) |> ExtendYears(Years)
    SDatAge <- SDatAge |> ExtendSims(nSim) |> ExtendYears(Years)
  }

  MeanAtAgeList <- MeanAtAge |> Array2List(pos = 1)
  SDatAgeList <- SDatAge |> Array2List(pos = 1)


  if (silent) {
    ASKList <- purrr::map2(MeanAtAgeList, SDatAgeList, \(x, y)
    CalcAgeSizeKey_(x, y, Classes, TruncSD, Dist))
  } else {
    ASKList <- purrr::map2(MeanAtAgeList, SDatAgeList, \(x, y)
    CalcAgeSizeKey_(x, y, Classes, TruncSD, Dist),
    .progress = list(
      type = "iterator",
      format = "Calculating Age-Size Key {cli::pb_bar} {cli::pb_percent}",
      clear = TRUE
    )
    )
  }

  AgeClasses <- dimnames(MeanAtAge)[['Age']]
  
  ASKList <- purrr::map(ASKList, \(ask) {
    if (is.null(dimnames(ask))) {
      dd <- dim(ask)
      dimnames(ask) <- list(
        Age=AgeClasses,
        Class=Classes,
        Year=dimnames(MeanAtAgeList[[1]])$Year[1:dd[3]]
      )
    }
    ask
  })
  
  List2Array(ASKList, "Sim", pos=1) 
}


