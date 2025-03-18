
# Length ----
#' @export
setMethod('plot', 'length', function(x,
                                     size.mean=1.5,
                                     col.mean='darkblue',
                                     maxSim=5,
                                     maxTS=3,
                                     type=c('MeanAtAge', 'ASK'),
                                     bySim=FALSE,
                                     Ages=NULL,
                                     nsim=NULL,
                                     TimeSteps=NULL,
                                     seed=NULL,
                                     ASK=TRUE,
                                     silent=FALSE) {

  type <- match.arg(type)

  Length <- x

  chk <- Check(Length)
  if (!chk@populated)
    Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK, silent)


  if (type=='MeanAtAge') {
    # Mean Length-at-Age
    dd <- dim(Length@MeanAtAge)
    nsim <- dd[1]
    nage <- dd[2]
    nTS <- dd[3]

    UnitsAge <- attributes(Length@MeanAtAge)$UnitsAge
    UnitsLength <- attributes(Length@MeanAtAge)$Units
    TimeSteps <- attributes(Length@MeanAtAge)$TimeSteps
    if (is.null(TimeSteps))
      TimeSteps <- 1:nTS

    if (length(TimeSteps)>nTS)
      TimeSteps <- TimeSteps[1:nTS]

    MeanAtAge <- array2DF(Length@MeanAtAge)
    MeanAtAge[,1] <- 1:nsim
    MeanAtAge[,2] <- rep(0:(nage-1), each=nsim)
    MeanAtAge[,3] <- rep(TimeSteps, each=nsim*nage)
    colnames(MeanAtAge) <- c('Sim', 'Age','TimeStep', 'Value')


    keepSim <- min(nsim, maxSim)
    keepTS <- min(nTS, maxTS)

    keepSim <- 1:keepSim
    keepTS <- seq(from=1, to=nTS, length.out=keepTS)
    MeanAtAge <- MeanAtAge |>
      dplyr::filter(Sim %in% keepSim,
                    TimeStep %in% TimeSteps[keepTS]
      )

    p <- ggplot2::ggplot(MeanAtAge,
                         ggplot2::aes(x=Age, y=Value, group=Sim)) +
      ggplot2::geom_line(size=size.mean, color=col.mean) +
      ggplot2::expand_limits(y=0)

    if (nsim==1)
      bySim <- FALSE

    byTS <- TRUE
    if (nTS==1)
      byTS <- FALSE

    if (bySim & byTS) {
      if (!silent)
        cli::cli_alert('Faceting by Simulation (rows) and TimeSteps (columns)')
      p <- p + ggplot2::facet_grid(Sim~TimeStep)
    }

    if (!bySim & byTS) {
      if (!silent)
        cli::cli_alert('Faceting by Time Steps')
      p <- p + ggplot2::facet_wrap(~TimeStep)
    }

    if (bySim & !byTS) {
      if (!silent)
        cli::cli_alert('Faceting by Simulation')
      p <- p + ggplot2::facet_wrap(~Sim)
    }


    if (!is.null(UnitsAge)) {
      xlab <- paste0('Ages (', UnitsAge, ')')
    } else {
      xlab <- 'Ages'
    }

    if (!is.null(UnitsLength)) {
      ylab <- paste0('Length (', UnitsLength, ')')
    } else {
      ylab <- 'Length'
    }


    p <- p + ggplot2::labs(x=xlab, y=ylab) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 1)) +
      ggplot2::theme_bw()
  } else {

    dd <- dim(Length@ASK)
    nsim <- dd[1]
    nage <- dd[2]
    nbin <- dd[3]
    nTS <- dd[4]

    UnitsAge <- attributes(Length@MeanAtAge)$UnitsAge
    UnitsLength <- attributes(Length@MeanAtAge)$UnitsLength
    TimeSteps <- attributes(Length@MeanAtAge)$TimeSteps
    Classes <- Classes(Length)
    nbin <- length(Classes)

    if (is.null(TimeSteps))
      TimeSteps <- 1:nTS

    if (length(TimeSteps)>nTS)
      TimeSteps <- TimeSteps[1:nTS]

    ASK <- array2DF(Length@ASK)
    ASK[,1] <- 1:nsim
    ASK[,2] <- rep(0:(nage-1), each=nsim)
    ASK[,3] <- rep(Classes, each=nsim*nage)
    ASK[,4] <- rep(TimeSteps, each=nsim*nage*nbin)
    colnames(ASK) <- c('Sim', 'Age', 'Length Class', 'TimeStep', 'Value')

    keepSim <- min(nsim, maxSim)
    keepTS <- min(nTS, maxTS)

    keepSim <- 1:keepSim
    keepTS <- seq(from=1, to=nTS, length.out=keepTS)
    ASK <- ASK |>
      dplyr::filter(Sim %in% keepSim,
                    TimeStep %in% TimeSteps[keepTS]
      )

    p <- ggplot2::ggplot(ASK,
                         ggplot2::aes(x=`Length Class`, y=Value, group=Sim)) +
      ggplot2::facet_wrap(~Age, scales='free_y') +
      ggplot2::geom_line()


  }


 p


})


# Weight ----
#' @export
setMethod('plot', 'weight', function(x,...) {
  cli::cli_alert_danger('plot({.val {firstup(class(x))}}) not done yet!')
})

# NaturalMortality ----
#' @export
setMethod('plot', 'naturalmortality', function(x,...) {
  cli::cli_alert_danger('plot({.val {firstup(class(x))}}) not done yet!')
})


# Maturity ----
#' @export
setMethod('plot', 'maturity', function(x,...) {
  cli::cli_alert_danger('plot({.val {firstup(class(x))}}) not done yet!')
})

# Fecundity ----
#' @export
setMethod('plot', 'fecundity', function(x,...) {
  cli::cli_alert_danger('plot({.val {firstup(class(x))}}) not done yet!')
})

# SRR ----
#' @export
setMethod('plot', 'srr', function(x, MaxAge=10, nHistTS=20, nProjTS=15, nsim=5, ...) {


  cli::cli_alert_danger('plot({.val {firstup(class(x))}}) not done yet!')
#
#   SRR <- x
#   chk <- Check(SRR)
#   if (!chk@populated)
#     SRR <- Populate(SRR, MaxAge, nHistTS, nProjTS, nsim)
#
#   # SRR
#   S0 <- 1
#   S <- seq(0, S0, length.out=30)
#   ExpectedRecruitment <- GenerateSRR(SRR@Model, SRR@Pars, S, S0)
#
#   Model <- SRR@Model
#   Pars <- SRR@Pars




  # Recruitment Deviations



})

# SRR ----
#' @export
setMethod('plot', 'spatial', function(x, ...) {


  cli::cli_alert_danger('plot({.val {firstup(class(x))}}) not done yet!')




})
