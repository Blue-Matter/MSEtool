#' SRR Class
#'
#' An S4 class defining stock–recruitment relationships and recruitment
#' variability for a [Stock()] object.
#'
#' @slot Pars Named list of parameters defining the stock–recruit model.
#' @slot Model Character string or function identifying the SRR model.
#' @slot R0 Unfished recruitment.
#' @slot SD Standard deviation of recruitment deviations.
#' @slot AC Autocorrelation of recruitment deviations.
#' @slot SPFrom Spawning biomass source used by the SRR.
#' @slot TruncSD Truncation (in SD units) of recruitment deviations.
#' @slot RecDevInit Recruitment deviations for initial age structure.
#' @slot RecDevHist Recruitment deviations for historical time steps.
#' @slot RecDevProj Recruitment deviations for projection time steps.
#' @slot SpawnTimeFrac Fraction of the time step when spawning occurs.
#' @slot RelRecFun Optional relative recruitment function.
#' @slot Units Scaling factor for recruitment.
#' @slot Misc Miscellaneous list.
#'
#' @include class-unions.R
setClass(
  "srr",
  slots = c(
    Pars = "list",
    Model = "fun.char",
    R0 = "num.array.null",
    SD = "num.array.null",
    AC = "num.array.null",
    SPFrom = "char.num",
    TruncSD = "num.null",
    RecDevInit = "num.array.list",
    RecDevHist = "num.array.list",
    RecDevProj = "num.array.list",
    SpawnTimeFrac = "numeric",
    RelRecFun = "fun.char",
    Units = "numeric",
    Misc = "list"
  )
)


setValidity("srr", function(object) {
  TRUE
})