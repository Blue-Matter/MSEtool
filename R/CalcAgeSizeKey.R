#' Calculate an Age-Size Key
#'
#' Generates an Age-Size key given mean and standard deviation of size-at-age. The size-at-age
#' can be normally or log-normally distributed. By default the distribution is truncated at 2
#' standard deviations.
#'
#' @param MeanAtAge The mean size at age. Either a numeric vector of length `nage`, a 2D array with
#' dimensions `c(nSim, nage)`, or a 3D array with dimensions `c(nSim, nage, nTS)`.
#' Alternatively, can be a [Length()] object, in which case no other arguments are needed.
#' @param CVatAge The coefficient of variation (CV) at age. Same structure as `MeanAtAge`.
#' @param Classes A numeric vector with the midpoints of the size classes for the age-size key.
#' @param TruncSD Numeric value indicating the number of standard deviations
#' where the distribution is truncated. Use high values to approximate a non-truncated distribution
#' @param Dist The distribution of `MeanAtAge`. Character, either `normal` or `lognormal`
#' @param AgeClasses Optional. Numeric vector of values for the age classes. Defaults to `0:(nage-1)`.
#' @return A 4D array with dimensions `nSim`, `nage`, `nClasses`, and `nTS`
#'
#' @export
CalcAgeSizeKey <- function(MeanAtAge,
                           CVatAge,
                           Classes,
                           TruncSD = 2,
                           Dist = c("normal", "lognormal"),
                           AgeClasses = NULL,
                           silent = FALSE,
                           type = "Length") {
  Dist <- match.arg(Dist)

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

  nSim <- max(dim_MeanAtAge[1], dim_SDatAge[1]) # maximum number of simulations

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

  if (is.null(AgeClasses)) {
    AgeClasses <- 0:(nage - 1)
  }

  ListDimNames <- list(
    Age = AgeClasses,
    Class = Classes,
    Year = dimnames(MeanAtAgeList[[1]])$Year
  )

  ASK <- List2Array(ASKList, "Sim", "Age", ListDimNames) |>
    aperm(c("Sim", "Age", "Class", "Year"))

  # attributes(ASK)$Classes <- Classes
  # attributes(ASK)$Ages <- Ages
  ASK
}
