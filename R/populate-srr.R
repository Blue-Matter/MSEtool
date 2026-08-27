#' Populate an SRR Object
#'
#' Populate a `SRR` object by generating recruitment deviations, 
#' and structuring values across simulation replicates and years.
#'
#' @param SRR A [SRR()] object to populate.
#' @param Ages An [Ages()] object defining age classes.
#' @param CurrentYear Numeric. Last historical year of the operating model.
#' @param Years Numeric vector of model years.
#' @param nSim Integer. Number of simulation replicates.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#' @details
#' `PopulateSRR()` handles population of stock-recruitment parameters. Steps
#' include:
#'
#' * Structuring model parameters across `nSim` and `Years`
#' * Resolving the SRR model class and relative recruitment function
#' * Initializing `R0`, `SD`, and `AC` parameters
#' * Generating historical and projection recruitment deviations
#'
#' @return
#' A populated [SRR()] object.
#'
#' @seealso
#' [Populate()], [PopulateStock()]
#'
#' @examples
#' \dontrun{
#' # Assuming `Ages` object exists
#' SRR_obj <- SRR()
#' SRR_pop <- PopulateSRR(
#'   SRR_obj,
#'   Ages = Ages,
#'   CurrentYear = 2025,
#'   Years = 2000:2030,
#'   nSim = 10,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateSRR <- function(SRR,
                        Ages = NULL,
                        CurrentYear = NULL,
                        Years = NULL,
                        nSim = 5,
                        seed = NULL,
                        silent = FALSE,
                        force = FALSE) {
  
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  nSim  <- .GetNSim(SRR, nSim)
  
  if (is.null(CurrentYear)) 
    CurrentYear <-  as.numeric(format(Sys.Date(), "%Y"))
  
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, CurrentYear, Years, nSim, seed)
  
  MaxAge <- Ages@MaxAge
  
  if (is.null(MaxAge)) 
    cli::cli_abort("`MaxAge` cannot be NULL")
  
  if (is.null(CurrentYear)) 
    cli::cli_abort("`CurrentYear` cannot be NULL")
  

  if (is.null(Years)) 
    cli::cli_abort("`Years` cannot be NULL")
  
  if (is.null(nSim)) {
    cli::cli_alert_info("`nSim` not specified. Assuming `nSim=1` and no recruitment process error.")
    nSim <- 1
  }
  
  tYears <- floor(Years)
  HistTS <- Years[tYears <= CurrentYear]
  ProjTS <- Years[tYears > CurrentYear]
  nHistTS <- length(HistTS)
  nProjTS <- length(ProjTS)
  nInitRecDev <- length(Ages@Classes) - 1

  .CheckRecDevDim(SRR@RecDevInit, nInitRecDev, "RecDevInit",
                  "`length(Ages@Classes) - 1`")
  .CheckRecDevDim(SRR@RecDevHist, nHistTS, "RecDevHist",
                  "the number of historical years")
  .CheckRecDevDim(SRR@RecDevProj, nProjTS, "RecDevProj",
                  "the number of projection years")

  if ((.CheckDigest(SRR, argList) && !force) | EmptyObject(SRR))
    return(SRR)
  
  .SetSeed(seed)
  
  # TODO - should add some checks here to make sure parameters/model are ok
  SRR@Pars  <- .StructurePars(Pars = SRR@Pars, nSim, Years)
  SRR@Model <- .FindModel(SRR)
  
  # Load the Relative Recruitment function
  if (is.character(SRR@Model) && is.null(SRR@RelRecFun)) 
    SRR@RelRecFun <- paste(SRR@Model, "RelRec", sep = "_")
  
  # checks 
  names <- c('R0', 'SD', 'AC')
  defaults <- c(1000, 0.4, 0)
  for (i in seq_along(names)) 
    SRR <- .CheckSRRPars(SRR, names[i], defaults[i])
  
  pars   <- .StructurePars(Pars = list(SRR@R0, SRR@SD, SRR@AC), nSim, Years)
  SRR@R0 <- pars[[1]] 
  SRR@SD <- pars[[2]][, 1, drop = FALSE] # only one time step for now
  SRR@AC <- pars[[3]][, 1, drop = FALSE] # only one time step for now
  SRR@AC[!is.finite(SRR@AC)] <- 0
  SRR@SD[SRR@SD == 0] <- 1E-6 # for reproducibility in rnorm
  
  EmptyObjects <- c(
    EmptyObject(SRR@RecDevInit),
    EmptyObject(SRR@RecDevHist),
    EmptyObject(SRR@RecDevProj)
  )
  
  if (all(!EmptyObjects)) {
    dd <- dim(SRR@RecDevInit)
    dimnames(SRR@RecDevInit) <- list(
      Sim = 1:dd[1],
      Age = Ages@Classes[-1]
    )
    
    dd <- dim(SRR@RecDevHist)
    dimnames(SRR@RecDevHist) <- list(
      Sim = 1:dd[1],
      Year = HistTS
    )
    
    dd <- dim(SRR@RecDevProj)
    dimnames(SRR@RecDevProj) <- list(
      Sim = 1:dd[1],
      Year = ProjTS
    )
    return(.SetDigest(SRR, argList))
  }
  
  RecDeviations <- GenRecDevs(
    SD = SRR@SD,
    AC = SRR@AC,
    TruncSD = SRR@TruncSD,
    Ages,
    HistTS,
    ProjTS,
    nSim = nSim,
    RecDevInit = SRR@RecDevInit,
    RecDevHist = SRR@RecDevHist,
    RecDevProj = SRR@RecDevProj
  )
  
  SRR@RecDevInit <- RecDeviations$RecDevInit
  dimnames(SRR@RecDevInit) <- list(
    Sim = 1:nrow(SRR@RecDevInit),
    Age = Ages@Classes[-1]
  )
  
  SRR@RecDevHist <- RecDeviations$RecDevHist
  dimnames(SRR@RecDevHist) <- list(
    Sim = 1:nrow(SRR@RecDevHist),
    Year = HistTS
  )
  
  SRR@RecDevProj <- RecDeviations$RecDevProj
  dimnames(SRR@RecDevProj) <- list(
    Sim = 1:nrow(SRR@RecDevProj),
    Year = ProjTS
  )
  .SetDigest(SRR, argList)
}


.CheckRecDevDim <- function(x, n, label, expected) {
  if (is.null(x)) return(invisible(TRUE))
  actual <- if (!is.null(dim(x))) dim(x)[length(dim(x))] else length(x)
  if (actual != n)
    cli::cli_abort(c(
      "x" = "{.arg {label}} has a trailing dimension of length {.val {actual}} but {.val {n}} is expected.",
      "i" = "The trailing dimension of {.arg {label}} must match {expected} ({.val {n}})."
    ))
  invisible(TRUE)
}

.CheckSRRPars <- function(SRR, name='R0', default=1000) {
  val <- slot(SRR, name)
  if (is.null(val)) {
    if (name !='AC') {
      cli::cli_alert_danger('Warning: {.val {name}} is missing in {.val SRR}')
      cli::cli_alert_info('Using default value: {.val {default}}')  
    }
    slot(SRR, name) <- default
  }
  SRR
}
