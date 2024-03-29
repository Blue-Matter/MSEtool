% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data_Plotting.R
\name{plot.Data}
\alias{plot.Data}
\title{Plot Data object}
\usage{
\method{plot}{Data}(
  x,
  wait = TRUE,
  i = 1,
  plots = "all",
  rmd = FALSE,
  head = "##",
  tplot = 25,
  ...
)
}
\arguments{
\item{x}{An object of class Data}

\item{wait}{Logical. Wait for key press before next plot?}

\item{i}{iteration number for the Data object.}

\item{plots}{Character. What plots to show? \code{all}, \code{TS}, \code{CAA}, \code{CAL}, \code{PD}
for all plots, time-series, catch-at-age, catch-at-length, and
probability distributions respectively}

\item{rmd}{Logical. Used in a rmd file?}

\item{head}{Character. Heading for rmd file. Default is '##' (second level heading)}

\item{tplot}{Integer. Number of plots per page. Default 25}

\item{...}{Not used}
}
\description{
Creates plots of the Data object in the R console. Wrapper for \code{summary(Data)}
}
