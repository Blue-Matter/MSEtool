defaultplotPars <- function(plotPars=NULL) {

  out <- list(col='darkgray',
              breaks=10,
              axes=FALSE,
              cex.main=1,
              lwd=2,
              cex.lab=1.25)

  nms <- names(out)[names(out) %in% names(plotPars)]
  if (!is.null(plotPars))
    for (nm in nms) out[nm] <- plotPars[nm]
  out
}

getStockPars <- function(Object, nsim, nyears, proyears, seed) {
  Pars <- list()
  SampCpars <- list()
  if (class(Object) == 'OM') {
    nsim <- Object@nsim
    nyears <- Object@nyears
    proyears <- Object@proyears
    CurrentYr <- Object@CurrentYr
    SampCpars <- if(length(Object@cpars)>0) SampCpars <- SampleCpars(Object@cpars, nsim, silent=TRUE)
    seed <- Object@seed
    Pars$Stock <- SampleStockPars(Object, nsim, nyears, proyears, SampCpars,
                                  msg=FALSE)
  } else if (class(Object) =='Stock') {
    Pars$Stock <- SampleStockPars(Object, nsim, nyears, proyears, SampCpars,
                                  msg=FALSE)
    Pars$Name <- gsub(" ", "_", Object@Name)
    CurrentYr <- nyears
  } else if (class(Object) =='Hist') {
    nsim <- Object@OM@nsim
    Pars <- Object@SampPars
    CurrentYr <- Object@OM@CurrentYr
    nyears <- nrow(Object@SampPars$Fleet$Find)
    proyears <- ncol(Object@SampPars$Stock$Linfarray) - nyears
  } else if (class(Object) =='list') {
    Pars <- Object
    nsim <- length(Pars$Stock$D)
    if (!is.null(Pars$Fleet)) {
      nyears <- nrow(Pars$Fleet$Find)
      proyears <- ncol(Pars$Stock$Linfarray) - nyears
    }

    CurrentYr <- nyears
  } else {
    stop('Object must be class `OM`, `Stock`, `Hist` or list from `Hist@SampPars`',
        call.=FALSE)
  }
  list(Pars=Pars, nsim=nsim, nyears=nyears, proyears=proyears,
       seed=seed, CurrentYr=CurrentYr)
}

plot_sample_hist <- function(Vars, Pars, element="Stock", plotPars, its, nsamp) {
  ncol <- ceiling(sqrt(nrow(Vars)))
  nrow <- ceiling(nrow(Vars)/ncol)

  par(mfrow=c(nrow,ncol))
  for (i in 1:nrow(Vars)) {
    hist2(Pars[[element]][[Vars$Slot[i]]], col=plotPars$col, axes=plotPars$axes,
          main=Vars$Name[i], breaks=plotPars$breaks)
    abline(v=Pars[[element]][[Vars$Slot[i]]][its], col=1:nsamp, lwd=plotPars$lwd)
    axis(side=1)
  }
}


plot_sample_ts <- function(Vars, Pars, element="Stock", plotPars, its, nsamp,
                           nyears, proyears, CurrentYr) {
  ncol <- ceiling(sqrt(nrow(Vars)))
  nrow <- ceiling(nrow(Vars)/ncol)

  years <- c(seq(-nyears+1, 0, by=1), seq(1, proyears,1))
  if (CurrentYr > 1900) {
    years <- years + CurrentYr
  } else {
    CurrentYr <- 0
  }
  par(mfrow=c(nrow,ncol), oma=c(2,3,1,1), mar=c(3,2,2,2))
  for (i in 1:nrow(Vars)) {
    matplot(years, t(Pars[[element]][[Vars$Slot[i]]][its,]), type="l", bty="l",
            main=Vars$Name[i], lwd=plotPars$lwd, lty=1, ylab=Vars$Name[i], xlab="Year",
            las=1, xpd=NA)
    abline(v=CurrentYr, col="darkgray", lty=2)
  }
}


update_plot_num <- function(maxplot, plot.num) {
  if (all(is.na(plot.num))) plot.num <- 1:maxplot
  if (max(plot.num)>maxplot) {
    message('Only ', maxplot,' plots available. Ignoring plot.num > ', maxplot)
    plot.num <- plot.num[plot.num<=maxplot]
  }
  plot.num
}

plot.Depletion <- function(Object, nsamp=3, nsim=200,
                           nyears=50, proyears=28, plot.num=NA,
                           plotPars=NULL, seed=101) {

  plotPars <- defaultplotPars(plotPars)
  List <- getStockPars(Object, nsim, nyears, proyears, seed)
  Pars <- List$Pars
  nsim <- List$nsim
  set.seed(List$seed)
  its <- sample(1:nsim, nsamp)

  Vars <- data.frame(Slot=c('D',
                            'Fdisc'),
                     Name=c('Depletion (D)',
                            'Discard mortality (Fdisc)'))

  plot_sample_hist(Vars, Pars, element='Stock', plotPars, its, nsamp)
}

plot.Growth <- function(Object, nsamp=3, nsim=200,
                        nyears=50, proyears=28, plot.num=NA,
                        plotPars=NULL, seed=101) {

  plotPars <- defaultplotPars(plotPars)
  List <- getStockPars(Object, nsim, nyears, proyears, seed)
  set.seed(List$seed)
  its <- sample(1:List$nsim, nsamp)

  plot.num <- update_plot_num(3, plot.num)

  # Sampled Parameters
  if (1 %in% plot.num) {
    Vars <- data.frame(Slot=c('Linf',
                              'Linfsd',
                              'K',
                              'Ksd',
                              't0'),
                       Name=c('Asymptotic length (Linf)',
                              'Linf interannual variability (Linfsd)',
                              'vB growth coefficient (K)',
                              'K interannual variability (Ksd)',
                              'Age at length 0 (t0)'))
    plot_sample_hist(Vars, List$Pars, element='Stock', plotPars, its, nsamp)
  }

  # Time-series
  if (2 %in% plot.num) {
    Vars <- data.frame(Slot=c('Linfarray',
                              'Karray'),
                       Name=c('Annual asymptotic length (Linf)',
                              'Annual growth coefficient (K)'))
    plot_sample_ts(Vars, List$Pars, element="Stock", plotPars, its, nsamp,
                   List$nyears,  List$proyears, List$CurrentYr)
  }

  Pars <- List$Pars
  # Growth curves
  if (3 %in% plot.num) {
    par(mfrow=c(1,3), oma=c(2,3,1,1), mar=c(3,2,1,1))
    fstYr <- Pars$Stock$Len_age[its,,1]
    curYr <- Pars$Stock$Len_age[its,,nyears]
    lstYr <- Pars$Stock$Len_age[its,,proyears+nyears]
    MaxL <- max(Pars$Stock$Len_age)
    maxage <- Pars$Stock$maxage
    matplot(0:maxage, t(fstYr), type="l", bty="l", main="First historical year", ylim=c(0, MaxL),
            xlab="Age", ylab="Length", cex.lab=2, lwd=plotPars$lwd, lty=1, xpd=NA)
    matplot(0:maxage, t(curYr), type="l", bty="l", main="Last historical year", ylim=c(0, MaxL),
            axes=plotPars$axes, xlab="Age", ylab="", cex.lab=2, lwd=plotPars$lwd, lty=1, xpd=NA)
    axis(side=1)
    axis(side=2, labels=FALSE)
    matplot(0:maxage, t(lstYr), type="l", bty="l", main="Last projected year", ylim=c(0, MaxL),
            axes=plotPars$axes, xlab="Age", ylab="", cex.lab=2, lwd=plotPars$lwd, lty=1, xpd=NA)
    axis(side=1)
    axis(side=2, labels=FALSE)
  }
}

plot.Maturity <- function(Object, nsamp=3, nsim=200,
                          nyears=50, proyears=28, plot.num=NA,
                          plotPars=NULL, seed=101) {

  plotPars <- defaultplotPars(plotPars)
  List <- getStockPars(Object, nsim, nyears, proyears, seed)
  set.seed(List$seed)
  its <- sample(1:List$nsim, nsamp)

  plot.num <- update_plot_num(2, plot.num)

  # Sampled Parameters
  if (1 %in% plot.num) {

    # age-at-maturity is time-varying
    # Vars <- data.frame(Slot=c('L50',
    #                           'L95',
    #                           'ageM',
    #                           'age95'),
    #                    Name=c('Length at 50% maturity (L50)',
    #                           'Length at 95% maturity (L95)',
    #                           'Age at 50% maturity (A50)',
    #                           'Age at 95% maturity (A95)'))

    Vars <- data.frame(Slot=c('L50',
                              'L95'),
                       Name=c('Length at 50% maturity (L50)',
                              'Length at 95% maturity (L95)'))

    plot_sample_hist(Vars, List$Pars, element='Stock', plotPars, its, nsamp)
  }


  if (2 %in% plot.num) {
    Pars <- List$Pars
    par(mfrow=c(3,2), oma=c(2,3,1,1), mar=c(3,2,1,1))

    # first year
    yr <- 1
    slope <- log(19)/(Pars$Stock$L95array[,yr]-Pars$Stock$L50array[,yr])
    Ls <- seq(0, to=max(Pars$Stock$Linfarray[its,]), length.out=200)
    maxage <- Pars$Stock$maxage

    Mat_len <- sapply(its, function(X)  plogis(Ls, Pars$Stock$L50array[X,yr], 1/slope[X]))
    matplot(Ls, Mat_len, type="l", bty="l", main="First Year",
            lwd=plotPars$lwd, lty=1,
            ylab="Probability", xlab="", ylim=c(0,1), xpd=NA, cex.lab=plotPars$cex.lab, las=1)

    matplot(0:maxage, t(Pars$Stock$Mat_age[its,,nyears]), type="l", bty="l",
            main="First Year", lwd=plotPars$lwd,
            lty=1, axes=FALSE, xlim=c(0, max(Pars$Stock$maxage)), ylab="",
            xlab="", ylim=c(0,1), xpd=NA, cex.lab=plotPars$cex.lab, las=1)
    axis(side=1)
    axis(side=2, labels=FALSE)

    # last historical year
    yr <- nyears
    slope <- log(19)/(Pars$Stock$L95array[,yr]-Pars$Stock$L50array[,yr])
    Ls <- seq(0, to=max(Pars$Stock$Linfarray[its,]), length.out=200)
    maxage <- Pars$Stock$maxage

    Mat_len <- sapply(its, function(X)  plogis(Ls, Pars$Stock$L50array[X,yr], 1/slope[X]))
    matplot(Ls, Mat_len, type="l", bty="l", main="Last Historical Year",
            lwd=plotPars$lwd, lty=1,
            ylab="Probability", xlab="", ylim=c(0,1), xpd=NA,
            cex.lab=plotPars$cex.lab, las=1)

    matplot(0:maxage, t(Pars$Stock$Mat_age[its,,nyears]), type="l", bty="l",
            main="Last Historical Year", lwd=plotPars$lwd,
            lty=1, axes=FALSE, xlim=c(0, max(Pars$Stock$maxage)), ylab="",
            xlab="", ylim=c(0,1), xpd=NA, cex.lab=plotPars$cex.lab, las=1)
    axis(side=1)
    axis(side=2, labels=FALSE)

    # last projection year
    yr <- nyears + proyears
    slope <- log(19)/(Pars$Stock$L95array[,yr]-Pars$Stock$L50array[,yr])
    Ls <- seq(0, to=max(Pars$Stock$Linfarray[its,]), length.out=200)
    maxage <- Pars$Stock$maxage

    Mat_len <- sapply(its, function(X)  plogis(Ls, Pars$Stock$L50array[X,yr], 1/slope[X]))
    matplot(Ls, Mat_len, type="l", bty="l", main="Last Projection Year",
            lwd=plotPars$lwd, lty=1,
            ylab="Probability", xlab="Length", ylim=c(0,1), xpd=NA,
            cex.lab=plotPars$cex.lab, las=1)

    matplot(0:maxage, t(Pars$Stock$Mat_age[its,,nyears]), type="l", bty="l",
            main="Last Projection Year", lwd=plotPars$lwd,
            lty=1, axes=FALSE, xlim=c(0, max(Pars$Stock$maxage)), ylab="",
            xlab="Age", ylim=c(0,1), xpd=NA, cex.lab=plotPars$cex.lab, las=1)
    axis(side=1)
    axis(side=2, labels=FALSE)
  }
}

plot.NaturalMortality <- function(Object, nsamp=3, nsim=200,
                   nyears=50, proyears=28, plot.num=NA,
                   plotPars=NULL, seed=101) {

  plotPars <- defaultplotPars(plotPars)
  List <- getStockPars(Object, nsim, nyears, proyears, seed)
  set.seed(List$seed)
  its <- sample(1:List$nsim, nsamp)

  plot.num <- update_plot_num(4, plot.num)

  # Sampled Parameters
  if (1 %in% plot.num) {
    Vars <- data.frame(Slot=c('M',
                              'Msd'),
                       Name=c('Natural mortality (M)',
                              'M interannual variability (Msd)'))
    plot_sample_hist(Vars, List$Pars, element='Stock', plotPars, its, nsamp)
  }

  # Time-series
  if (2 %in% plot.num) {
    Vars <- data.frame(Slot=c('Marray'),
                       Name=c('Annual natural mortality'))
    plot_sample_ts(Vars, List$Pars, element="Stock", plotPars, its, nsamp,
                   List$nyears,  List$proyears, List$CurrentYr)
  }

  if (3 %in% plot.num) {
    Pars <- List$Pars
    maxage <- Pars$Stock$maxage
    par(mfrow=c(1,3))
    lims <- range(Pars$Stock$M_ageArray[its,, ])
    ylab <- "Natural Mortality"
    matplot(0:maxage, t(Pars$Stock$M_ageArray[its,,1]), type="l", lty=1, bty="l",
            lwd=plotPars$lwd, ylim=lims, ylab=ylab, las=1, xlab='', cex.lab=plotPars$cex.lab)
    mtext(side=3, "First historical year", cex=0.8, line=-1)
    mtext(side=1, "Age", line=2, cex=plotPars$cex.lab)

    matplot(0:maxage, t(Pars$Stock$M_ageArray[its,,nyears]), type="l", lty=1, bty="l",
            main="M-at-age", lwd=plotPars$lwd, ylim=lims, axes=FALSE,
            ylab="", xlab='', cex.lab=plotPars$cex.lab)
    mtext(side=3, "Last historical year", cex=0.8, line=-1)
    axis(side=1)
    mtext(side=1, "Age", line=2, cex=plotPars$cex.lab)
    matplot(0:maxage, t(Pars$Stock$M_ageArray[its,,nyears+proyears]), type="l", lty=1,
            bty="l", lwd=plotPars$lwd, ylim=lims, axes=FALSE,
            ylab="", xlab='', cex.lab=plotPars$cex.lab)
    mtext(side=3, "Last projected year", cex=0.8, line=-1)
    axis(side=1)
    mtext(side=1, "Age", line=2, cex=plotPars$cex.lab)
  }

  if (4 %in% plot.num) {
    par(mfrow=c(1,3))
    Pars <- List$Pars
    lims <- range(Pars$Stock$M_ageArray[its,, ])
    xlims <- range(Pars$Stock$Len_age[its,,])
    matplot(t(Pars$Stock$Len_age[its,,1]), t(Pars$Stock$M_ageArray[its,,1]), type="l",
            lty=1, bty="l", lwd=plotPars$lwd, ylim=lims, xlim=xlims, ylab="Natural Mortality",
            xlab="", las=1, cex.lab=plotPars$cex.lab)
    mtext(side=3, "First historical year", cex=0.8, line=-1)
    mtext(side=1, "Length", line=2, cex=plotPars$cex.lab)

    matplot(t(Pars$Stock$Len_age[its,,nyears]), t(Pars$Stock$M_ageArray[its,,nyears]),
            type="l", lty=1, bty="l", main="M-at-length", lwd=plotPars$lwd, ylim=lims,
            xlim=xlims, axes=FALSE, ylab="", xlab="", cex.lab=plotPars$cex.lab)
    axis(side=1)
    mtext(side=1, "Length", line=2, cex=plotPars$cex.lab)
    mtext(side=3, "Last historical year", cex=0.8, line=-1)

    matplot(t(Pars$Stock$Len_age[its,,nyears+proyears]),
            t(Pars$Stock$M_ageArray[its,,nyears+proyears]),
            type="l", lty=1, bty="l", lwd=plotPars$lwd, ylim=lims,
            axes=FALSE, xlim=xlims, ylab="", xlab="", cex.lab=plotPars$cex.lab)

    mtext(side=3, "Last projected year", cex=0.8, line=-1)
    axis(side=1)
    mtext(side=1, "Length", line=2, cex=plotPars$cex.lab)
  }
}


plot.Recruitment <- function(Object, nsamp=3, nsim=200,
                             nyears=50, proyears=28, plot.num=NA,
                             plotPars=NULL, seed=101) {

  plotPars <- defaultplotPars(plotPars)
  List <- getStockPars(Object, nsim, nyears, proyears, seed)
  set.seed(List$seed)
  its <- sample(1:List$nsim, nsamp)

  plot.num <- update_plot_num(2, plot.num)

  # Sampled Parameters
  if (1 %in% plot.num) {
    Vars <- data.frame(Slot=c('hs',
                              'procsd',
                              'AC'),
                       Name=c('Steepness (h)',
                              'Recruitment process error (Perr)',
                              'Auto-correlation (AC)'))
    plot_sample_hist(Vars, List$Pars, element='Stock', plotPars, its, nsamp)
  }

  # Time-series

  maxage <- List$Pars$Stock$maxage
  if (2 %in% plot.num) {
    Vars <- data.frame(Slot=c('Perr_y'),
                       Name=c('Rec Devs by Year'))
    plot_sample_ts(Vars, List$Pars, element="Stock", plotPars, its, nsamp,
                   List$nyears+maxage,  List$proyears, List$CurrentYr)
  }

}

plot.Spatial<- function(Object, nsamp=3, nsim=200,
                        nyears=50, proyears=28, plot.num=NA,
                        plotPars=NULL, seed=101) {

  plotPars <- defaultplotPars(plotPars)
  List <- getStockPars(Object, nsim, nyears, proyears, seed)
  set.seed(List$seed)
  its <- sample(1:List$nsim, nsamp)

  plot.num <- update_plot_num(2, plot.num)

  # Sampled Parameters
  if (1 %in% plot.num) {
    Vars <- data.frame(Slot=c('Size_area_1',
                              'Frac_area_1',
                              'Prob_staying'),
                       Name=c('Size of Area 1',
                              'Fraction Unfished Biomass in Area 1',
                              'Probability of Staying in Area 1'))
    plot_sample_hist(Vars, List$Pars, element='Stock', plotPars, its, nsamp)
  }
}


