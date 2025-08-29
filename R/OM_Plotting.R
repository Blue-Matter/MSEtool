
render_plot <- function(Object, Class, Stock=NULL, RMD=NULL, nsamp=3, nsim=200, nyears=50,
                        proyears=28, output_file=NULL, output_dir=getwd(),
                        quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                        plotPars=NULL, open=TRUE, dev=FALSE, parallel=FALSE) {

  SampCpars <- list() # empty list

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package \"rmarkdown\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (is.null(plotPars)) plotPars <- list(breaks=10, col="darkgray", axes=FALSE,
                                          cex.main=1, lwd=2)

  if (methods::is(Object, "OM")) {
    nsim <- Object@nsim
    nyears <- Object@nyears
    proyears <- Object@proyears
    SampCpars <- if(length(Object@cpars)>0) SampCpars <- SampleCpars(Object@cpars, nsim, silent=TRUE)
    set.seed(Object@seed)
    Stock <- SubOM(Object, "Stock")
    # Class <- "OM"
  }


  if (Class == "Stock") {
    if (is.null(title)) title <- "Stock Object Plots"
    Pars <- list()
    Pars$Stock <- SampleStockPars(Object, nsim, nyears, proyears, SampCpars,
                            msg=FALSE)
    name <- Object@Name
    name <- gsub(' ', '_', name)
    name <- gsub(':', '_', name)

    Pars$Name <- gsub(" ", "_", name)
  } else if (Class == "Fleet") {
    if (is.null(title)) title <- "Fleet Object Plots"
    if (!methods::is(Stock, "Stock"))
      stop("Must provide object of class 'Stock'", call. = FALSE)
    StockPars <- SampleStockPars(Stock, nsim, nyears, proyears, SampCpars,
                                 msg=FALSE)
    FleetPars <- SampleFleetPars(Object, StockPars, nsim, nyears, proyears,
                                 SampCpars, msg=FALSE)

    Pars <- list()
    Pars$Stock <- StockPars
    Pars$Fleet <- FleetPars
    Pars$Name <- gsub(" ", "_", Object@Name)
    Pars$CurrentYr <- Object@CurrentYr
    Pars$MPA <- Object@MPA

  } else if (Class == "Obs") {
    if (is.null(title)) title <- "Obs Object Plots"
    ObsPars <- SampleObsPars(Object, nsim, cpars=SampCpars, nyears=nyears, proyears=proyears)
    BMSY_B0bias <- array(rlnorm(nsim,
                                mconv(1, Object@BMSY_B0biascv), sdconv(1, Object@BMSY_B0biascv)),
                         dim = c(nsim))  # trial samples of BMSY relative to unfished

    ObsPars$BMSY_B0bias <- BMSY_B0bias

    Pars <- list()
    Pars$Obs <- ObsPars

  } else if (Class == "Imp") {
    if (is.null(title)) title <- "Imp Object Plots"
    ImpPars <- SampleImpPars(Object, nsim, cpars=SampCpars, nyears=nyears, proyears=proyears)
    Pars <- list()
    Pars$Imp <- ImpPars
  } else if (Class == "OM") {
    if (is.null(title)) title <- "OM Object Plots"
    message("Sampling Stock, Fleet, Obs, and Imp parameters")

    if (!parallel) dopar <- FALSE
    if (nsim>=48 & parallel) dopar <- TRUE
    if (nsim<48& parallel) dopar <- FALSE
    message("Running Historical Simulations")
    Hist <- Simulate(Object, silent=TRUE, parallel = dopar)

    Pars <- list(Stock=Hist@SampPars$Stock,
              Fleet=Hist@SampPars$Fleet,
              Obs=Hist@SampPars$Obs,
              Imp=Hist@SampPars$Imp)

    Pars$Hist <- Hist
    Pars$Name <- "OM"
    Pars$MPA <- Object@MPA
    Pars$CurrentYr <- Object@CurrentYr
  } else if (Class == "Hist") {
    Pars <- list()
    Pars$Hist <- Object

    Pars$CurrentYr <-  Object@OMPars$CurrentYr[1]
    nyears <- length(Object@Data@Year)
    if (is.null(title)) title <- "Historical Simulations"

  } else {
    stop("Object must be class 'Stock', 'Fleet', 'Obs', or 'Imp'", call.=FALSE)
  }

  if (Class !="Hist" & Class !="OM") {
    name <- Object@Name
    name <- gsub(' ', '_', name)
    name <- gsub(':', '_', name)
    Pars$Name <- gsub(" ", "_", name)
  }
  its <- sample(1:nsim, nsamp)
  # Pars <<- Pars
  Params <- list(
    title = title,
    Pars = Pars,
    plotPars=plotPars,
    tabs = tabs,
    its = its,
    nyears=nyears,
    proyears=proyears,
    date=NULL
  )


  outname <- paste0("_", RMD, ".html")
  if (Class !="Hist" & Class !="OM") {
    if (is.null(output_file)) output_file <- paste0(Pars$Name, outname)
  } else {
    if (is.null(output_file)) output_file <-  paste0(RMD, ".html")
  }
  message("Rendering HTML file")

  RMD <- paste0(RMD, ".Rmd")
  if (dev) {
    input <- file.path('inst/Rmd', Class, RMD)
  } else {
    input <- file.path(system.file(package = 'MSEtool'),'Rmd', Class, RMD)
  }

  knitr::knit_meta(class=NULL, clean = TRUE)
  rend <- try(rmarkdown::render(input, params=Params,
                                output_file=output_file,
                                output_dir=output_dir,
                                quiet=quiet), silent=TRUE)
  if (methods::is(rend,"try-error")) {
    print(rend)
  } else {
    message("Rendered ", output_file, " in ", output_dir)
    if (open) utils::browseURL(file.path(output_dir, output_file))
  }
}


#' @method plot character
#' @export
#' @keywords internal
plot.character <- function(x, Object, ...) {
  plot.pars(x, Object, ...)
}

#' @param Object An object of class `Stock` or `Fleet`
#' @param Stock An object of class `Stock` required for `Fleet` parameters
#' @rdname plot.Stock
#' @method plot pars
#' @export
plot.pars <- function(x, Object, Stock=NULL, nsamp=3, nsim=200, nyears=50,
                      proyears=28, output_file=NULL, output_dir=getwd(),
                      quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                      plotPars =NULL, html=FALSE, open=TRUE, dev=FALSE, ...) {

  StockDF <- data.frame(chr=c("M",
                              "Growth",
                              "Maturity",
                              "Recruitment",
                              "Spatial",
                              "Depletion"),
                        Class="Stock",
                        stringsAsFactors = FALSE)

 FleetDF <- data.frame(chr=c("Effort",
                              "Catchability",
                              "MPA",
                              "Selectivity"),
                        Class="Fleet",
                        stringsAsFactors = FALSE)

 DF <- dplyr::bind_rows(StockDF, FleetDF)

  if (!x %in% DF[,1])
    stop("Invalid argument. Valid arguments are: ", paste0(DF[,1], sep=" "), call.=FALSE)

  Class <- DF$Class[match(x, DF[,1])]
  if (!methods::is(Object, "OM") & !methods::is(Object, Class) & !methods::is(Object, "Hist"))
    stop("Incorrect class object for this parameter", call.=FALSE)


  if (x == "M") x <- "NaturalMortality"

  if (!html) {
    fun <- paste0('plot.', x)
    if (Class=='Stock')
      do.call(fun, list(Object=Object, nsamp=nsamp, plotPars=plotPars, ...))
    if (Class=='Fleet')
      do.call(fun, list(Object=Object, Stock=Stock, nsamp=nsamp, plotPars=plotPars, ...))
  } else {
    render_plot(Object=Object, Class=Class, Stock=Stock, RMD=x, nsamp=nsamp, nsim=nsim,
                nyears=nyears, proyears=proyears,
                output_file=output_file, output_dir=output_dir, quiet=quiet,
                tabs=tabs, title=title, date=date,
                plotPars=plotPars, open=open, dev=dev)
  }


}



#' @title Plot Operating Model Object
#'
#' @description Generate HTML reports with plots of operating model components ("Stock",
#' "Fleet", "Obs", and "Imp"), the historical simulations ("Hist"), or the complete OM ("OM").
#'
#' The individual component plots of objects of class `Stock` and `Fleet` can also be generated by
#' using the generic `plot.pars` function. See Examples below.
#'
#' @param x An object of class `Stock`, `Fleet`, `Obs`, `Imp`, `Hist`, or `OM`, OR one
#' of the following character strings for `Object` of class `Stock`: "M", "Growth", "Maturity", "Recruitment", "Spatial",
#' or "Depletion" and for `Object` of class `Fleet`: "Effort", "Catchability", "MPA",
#' and "Selectivity".
#' @param nsamp The number of random samples to show in the plot
#' @param nsim The number of simulations (only used for objects not of class `OM`)
#' @param nyears The number of historical years (only used for objects not of class `OM`)
#' @param proyears The number of projection years (only used for objects not of class `OM`)
#' @param output_file Name of the output html file (without file extension)
#' @param output_dir Output directory. Defaults to `getwd()`
#' @param quiet An option to suppress printing of the pandoc command line
#' @param tabs Include tabs in the HTML file?
#' @param title Optional title for the markdown report
#' @param date Optional date for the markdown report
#' @param plotPars A named list with options for plots:
#' \itemize{
#'   \item breaks - numeric. Number of breaks in histograms.
#'   \item col - character. Color of histograms.
#'   \item axes - logical. Include axes in histogram?
#'   \item cex.main - numeric. Size of main title in plots.
#'   \item lwd - numeric. Line width for time-series plots.
#' }
#' @param html Logical. Compile to a HTML report (TRUE) or print plots in R console (FALSE)
#' @param open Logical. Open the html file?
#' @param dev Logical. For development use only.
#' @param ... Not used
#'
#' @method plot Stock
#' @export
#' @examples
#' \dontrun{
#' # Plot Stock Object:
#' Stock <- MSEtool::Albacore
#' plot(Stock)
#'
#' # Individual plots:
#' plot("M", Stock)
#' plot("Growth", Stock)
#' plot("Maturity", Stock)
#' plot("Recruitment", Stock)
#' plot("Spatial", Stock)
#' plot("Depletion", Stock)
#'
#' # Plot Fleet Object
#' Fleet <- MSEtool::Generic_DecE
#' plot(Fleet, Stock)
#'
#' # Individual plots:
#' plot("Effort", Fleet, Stock)
#' plot("Catchability", Fleet, Stock)
#' plot("MPA", Fleet, Stock)
#' plot("Selectivity", Fleet, Stock)
#'
#'
#' # Plot Obs Object
#' Obs <- MSEtool::Imprecise_Unbiased
#' plot(Obs)
#'
#' # Plot Imp Object
#' Imp <- MSEtool::Overages
#' plot(Imp)
#'
#'
#' # Plot Hist Object
#' OM <- MSEtool::testOM
#' Hist <- Simulate(OM)
#' plot(Hist)
#'
#' # Plot OM Object
#' plot(OM)
#' }
plot.Stock <- function(x, nsamp=3, nsim=200, nyears=50,
                       proyears=28, output_file=NULL, output_dir=getwd(),
                       quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                       plotPars =NULL, open=TRUE, dev=FALSE, ...){

  render_plot(Object=x, Class="Stock", RMD='Stock', nsamp=nsamp, nsim=nsim,
              nyears=nyears, proyears=proyears,
              output_file=output_file, output_dir=output_dir, quiet=quiet,
              tabs=tabs, title=title, date=date,
              plotPars=plotPars, open=open, dev=dev)
}


#' @rdname plot.Stock
#' @method plot Fleet
#' @export
plot.Fleet <- function(x, Stock=NULL, nsamp=3, nsim=200, nyears=50,
                       proyears=28, output_file=NULL, output_dir=getwd(),
                       quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                       plotPars =NULL, open=TRUE, dev=FALSE, ...){
  if (!methods::is(Stock, "Stock") & !methods::is(x, "OM"))
    stop("Must provide object of class 'Stock'")

  render_plot(Object=x, Class="Fleet", Stock=Stock, RMD='Fleet', nsamp=nsamp, nsim=nsim,
              nyears=nyears, proyears=proyears,
              output_file=output_file, output_dir=output_dir, quiet=quiet,
              tabs=tabs, title=title, date=date,
              plotPars=plotPars, open=open, dev=dev)
}

#' @rdname plot.Stock
#' @method plot Obs
#' @export
plot.Obs <- function(x, nsamp=3, nsim=200, nyears=50,
                       proyears=28, output_file=NULL, output_dir=getwd(),
                       quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                       plotPars =NULL, open=TRUE, dev=FALSE, ...){

  render_plot(Object=x, Class="Obs", Stock=NULL, RMD='Obs', nsamp=nsamp, nsim=nsim,
              nyears=nyears, proyears=proyears,
              output_file=output_file, output_dir=output_dir, quiet=quiet,
              tabs=tabs, title=title, date=date,
              plotPars=plotPars, open=open, dev=dev)
}

#' @rdname plot.Stock
#' @method plot Imp
#' @export
plot.Imp <- function(x, nsamp=3, nsim=200, nyears=50,
                     proyears=28, output_file=NULL, output_dir=getwd(),
                     quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                     plotPars =NULL, open=TRUE, dev=FALSE, ...){

  render_plot(Object=x, Class="Imp", Stock=NULL, RMD='Imp', nsamp=nsamp, nsim=nsim,
              nyears=nyears, proyears=proyears,
              output_file=output_file, output_dir=output_dir, quiet=quiet,
              tabs=tabs, title=title, date=date,
              plotPars=plotPars, open=open, dev=dev)
}

#' @rdname plot.Stock
#' @method plot Hist
#' @export
plot.Hist <- function(x, nsamp=3, nsim=200, nyears=50,
                      proyears=28, output_file=NULL, output_dir=getwd(),
                      quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                      plotPars =NULL, open=TRUE, dev=FALSE, ...) {
  render_plot(Object=x, Class="Hist", Stock=NULL, RMD='Hist', nsamp=nsamp, nsim=nsim,
              nyears=nyears, proyears=proyears,
              output_file=output_file, output_dir=output_dir, quiet=quiet,
              tabs=tabs, title=title, date=date,
              plotPars=plotPars, open=open, dev=dev)
}

#' @rdname plot.Stock
#' @method plot OM
#' @export
plot.OM <- function(x, nsamp=3, nsim=200, nyears=50,
                    proyears=28, output_file=NULL, output_dir=getwd(),
                    quiet=TRUE, tabs=TRUE, title=NULL, date=NULL,
                    plotPars =NULL, open=TRUE, dev=FALSE, ...) {
  render_plot(Object=x, Class="OM", Stock=NULL, RMD='OM', nsamp=nsamp, nsim=nsim,
              nyears=nyears, proyears=proyears,
              output_file=output_file, output_dir=output_dir, quiet=quiet,
              tabs=tabs, title=title, date=date,
              plotPars=plotPars, open=open, dev=dev)


}


#' Wrapper for histogram function
#'
#' Produces a blank plot if all values in x are equal
#'
#' @param x A vector of values
#' @param col Colour of the histogram
#' @param axes Logical - should axes be included?
#' @param main Character - main title
#' @param breaks Number of breaks. See ?hist for more details
#' @param cex.main Text size of the main title
#'
#' @export
#'
hist2 <- function(x, col, axes=FALSE, main="", breaks=10,cex.main=1) {
  if (mean(x) == x[1]) {

    plot(mean(x)*c(0.9,1.1),c(0,1000),col="white",axes=F,main=main,xlab="",ylab="")
    axis(1)
    #hist(x, border="white", xlim=range(x),xlab="", ...)
    abline(v=mean(x),col=col,lwd=2)

  } else {
    col="dark grey"
    hist(x, border='white',xlab="",col=col,axes=axes,main=main,breaks=breaks, ylab="")
    graphics::grid()
  }
}


#' @importFrom graphics plot grid matplot
plot.default <- function(...) graphics::plot.default(..., panel.first = graphics::grid())
matplot <- function(...) graphics::matplot(..., panel.first = graphics::grid())
