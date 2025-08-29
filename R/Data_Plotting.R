# ---- Plot Data Object -----
#' Plot Data object
#'
#' Creates plots of the Data object in the R console. Wrapper for `summary(Data)`
#'
#' @param x An object of class Data
#' @param wait Logical. Wait for key press before next plot?
#' @param i iteration number for the Data object.
#' @param plots Character. What plots to show? `all`, `TS`, `CAA`, `CAL`, `PD`
#' for all plots, time-series, catch-at-age, catch-at-length, and
#' probability distributions respectively
#' @param rmd Logical. Used in a rmd file?
#' @param head Character. Heading for rmd file. Default is '##' (second level heading)
#' @param tplot Integer. Number of plots per page. Default 25
#' @param ...  Not used
#' @method plot Data
#' @export
plot.Data <- function(x, wait=TRUE, i=1, plots='all', rmd=FALSE, head="##", tplot=25, ...) {
  summary(x, wait, i, plots, rmd, head, tplot)
}

#' Boxplot of TAC recommendations
#'
#' @param x An object of class MSE
#' @param upq Upper quantile of TACs for max ylim
#' @param lwq Lower quantile of TACs for min ylim
#' @param ylim Optional numeric vector of length 2 to specify limits of y-axis.
#' @param outline Logical. Include outliers in plot?
#' @param col Optional colours to pass to \code{boxplot}
#' @param ...  Optional additional arguments passed to \code{boxplot}
#' @return Returns a data frame containing the information shown in the plot
#' @author A. Hordyk
#' @method boxplot Data
#' @export
boxplot.Data <- function(x, upq=0.9, lwq=0.1, ylim=NULL, outline = FALSE, col = NULL, ...) {
  Data <- updateMSE(x)
  if (!methods::is(Data,"Data"))  stop("Object must be of class 'Data'")

  if (all(is.na(Data@TAC))) {
    stop('Cannot plot TACs because nothing found in `Data@TAC`.\nUse `Report(Data)` to generate Data report', call. = FALSE)
  }

  tacs <- t(Data@TAC[, , 1])

  if (all(is.na(tacs))) {
    message("Nothing found in TAC slot")
    return(invisible(NULL))
  }
  units <- TRUE
  if (length(nchar(x@Units)) < 1) units <- FALSE
  MPs <- Data@MPs
  ind <- grep("ref", MPs)
  if (length(ind) > 0) {
    tacs <- tacs[, -ind, drop=FALSE]
    MPs <- MPs[-ind]
  }

  # exclude NAs
  nMPs <- dim(Data@TAC)[1]

  if (nMPs>1){
    allNAs <- colSums(apply(tacs, 2, is.na)) == nrow(tacs)
    tacs <- tacs[,!allNAs, drop=FALSE]
    MPs <- MPs[!allNAs]
    nMPs<-length(MPs)
  }

  if (nMPs>1) {
    if (is.null(col)) col <- rainbow(30)
    ord <- order(apply(tacs, 2, median, na.rm = TRUE))
    MPs <- MPs[ord]
    tacs <- tacs[, ord]
    ymax <- max(apply(tacs, 2, quantile, upq, na.rm = TRUE))
    ymin <- min(apply(tacs, 2, quantile, lwq, na.rm = TRUE))
    if (is.null(ylim)) ylim <- c(ymin, ymax)
    Median <- round(apply(tacs, 2, median, na.rm = TRUE), 2)
    SD <- round(apply(tacs, 2, sd, na.rm = TRUE), 2)
  } else {
    if (is.null(ylim)) ylim <- c(min(tacs), max(tacs))
    Median <- median(tacs)
    SD <- sd(tacs)
    tacs <- as.numeric(tacs)
    if (is.null(col)) col <- "darkgray"
  }

  par(mfrow = c(1, 1), oma = c(2, 4, 1, 0), mar = c(3, 3, 0, 0))
  if (nMPs>1) {
    boxplot(tacs, names = MPs, las = 1, col = col, outline = outline,
          frame = FALSE, ylim = ylim, horizontal = TRUE, ...)
    if (units) mtext(paste("TAC (", Data@Units, ")", sep = ""), side = 1, outer = T,
                     line = 0.5, cex = 1.25)
    if (!units) mtext("TAC (no units supplied)", side = 1, outer = T,
                      line = 0.5, cex = 1.25)
    mtext(side = 2, "Management Procedures", outer = TRUE, line = 3, cex = 1.25)
  } else {
    boxplot(tacs, names = MPs, las = 1, col = col, outline = outline,
            frame = FALSE, ylim = ylim, horizontal = FALSE, ...)
    if (units) mtext(paste("TAC (", Data@Units, ")", sep = ""), side = 2, outer = T,
                     line = 0.5, cex = 1.25)
    if (!units) mtext("TAC (no units supplied)", side = 2, outer = T,
                      line = 0.5, cex = 1.25)
    mtext(side = 3, MPs, outer = TRUE, line=-1, cex = 1.25, xpd=NA)
  }

  if (units) {
      data.frame(MP = MPs, Median = Median, SD = SD, Units = Data@Units)
  } else {
      data.frame(MP = MPs, Median = Median, SD = SD)
  }
}
