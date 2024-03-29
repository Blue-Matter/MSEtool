% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_definitions.R
\docType{class}
\name{MSE-class}
\alias{MSE-class}
\title{Class \code{'MSE'}}
\description{
A Management Strategy Evaluation object that contains information about
simulation conditions and performance of data-limited methods
}
\section{Slots}{

\describe{
\item{\code{Name}}{Name of the MSE object. Single value. Character string}

\item{\code{nyears}}{The number of years for the historical simulation. Single
value. Positive integer}

\item{\code{proyears}}{The number of years for the projections - closed loop
simulations. Single value. Positive integer}

\item{\code{nMPs}}{Number of management procedures simulation tested. Single value.
Positive integer.}

\item{\code{MPs}}{The names of the MPs that were tested. Vector of length nMPs.
Character strings.}

\item{\code{nsim}}{Number of simulations. Single value. Positive integer}

\item{\code{OM}}{Operating model parameters (last historical year used for
time-varying parameters). Data.frame with \code{nsim} rows}

\item{\code{Obs}}{Observation parameters (last historical year used for time-varying
parameters). Data.frame with \code{nsim} rows}

\item{\code{SB_SBMSY}}{Simulated spawning biomass relative to spawning BMSY over the
projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real
numbers}

\item{\code{F_FMSY}}{Simulated fishing mortality rate relative to FMSY over the
projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real
numbers}

\item{\code{N}}{Simulated total numbers over the projection. An array with
dimensions: nsim, maxage+1, nMPs, proyears, nareas. Non-negative real numbers.}

\item{\code{B}}{Simulated stock biomass over the projection. An array with
dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{SSB}}{Simulated spawning stock biomass over the projection. An array
with dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{VB}}{Simulated vulnerable biomass over the projection. An array with
dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{FM}}{Simulated fishing mortality rate over the projection. An array with
dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{SPR}}{Named list with equilibrium and dynamic SPR. Each element is an
array with dimensions: nsim, nMPs, proyears. Non-negative real numbers.}

\item{\code{Catch}}{Simulated catches (landings) over the projection. An array with
dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{Removals}}{Simulated removals (catch + discards) over the projection. An
array with dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{Effort}}{Simulated relative fishing effort in the projection years. An
array with dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{TAC}}{Simulated Total Allowable Catch prescribed by MPs. An array with
dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{TAE}}{Simulated Total Allowable Effort prescribed by MPs. An array with
dimensions: nsim, nMPs, proyears. Non-negative real numbers}

\item{\code{BioEco}}{Named list with bio-economic output Only used if bio-economic
parameters are included in OM}

\item{\code{RefPoint}}{Named list of annual MSY reference points MSY, FMSY, and
SBMSY. Array with dimensions: nsim, nMPs, nyears+proyears. Will be the same as
\code{Hist@Ref$ByYear} unless selectivity is changed by MP}

\item{\code{CB_hist}}{Simulated catches (landings) from the spool-up period. An
array with dimensions: nsim, nyears. Non-negative real numbers}

\item{\code{FM_hist}}{Simulated fishing mortality rate from the spool-up period. An
array with dimensions: nsim, nyears Non-negative real numbers}

\item{\code{SSB_hist}}{Simulated spawning stock biomass from the spool-up period. An
array with dimensions: nsim, nyears. Non-negative real numbers}

\item{\code{Hist}}{Information from the historical spool-up period. Object of class
\code{Hist}. Only contains slots \code{AtAge} and \code{TSdata} unless \code{extended=TRUE} in \code{runMSE}}

\item{\code{PPD}}{Posterior predictive data. List of \code{Data} objects at the end of
the projection period (length \code{nMPs})}

\item{\code{Misc}}{Miscellaneous output}
}}

\author{
T. Carruthers and A. Hordyk
}
\keyword{classes}
