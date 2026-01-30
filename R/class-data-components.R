
#' Catch Data Class
#'
#' S4 class defining catch (landings or discards) observations.
#'
#' @slot Name Character name of the dataset
#' @slot Value Numeric array of observed catch
#' @slot CV Numeric array of observation CVs
#' @slot Units Character units of measurement
#' @slot Ref Numeric array of reference values
#' @slot RefCV Numeric array of reference CVs
#'
#' @include class-unions.R
#'
#' @export
setClass(
  "catchdata",
  slots = c(
    Name   = "char.null",
    Value  = "array.null",
    CV     = "array.null",
    Units  = "char.null",
    Ref    = "array.null",
    RefCV  = "array.null"
  )
)


#' Effort Data Class
#'
#' S4 class defining fishing effort observations.
#'
#' @slot Name Character name of the dataset
#' @slot Value Numeric array of effort values
#' @slot CV Numeric array of observation CVs
#' @slot Units Character units of effort
#'
#' @include class-unions.R
#'
#' @export
setClass(
  "effortdata",
  slots = c(
    Name   = "char.null",
    Value  = "array.null",
    CV     = "array.null",
    Units  = "char.null"
  )
)


#' Indices Data Class
#'
#' S4 class defining abundance or biomass index observations.
#'
#' @slot Name Character name of the index
#' @slot Value Numeric array of index values
#' @slot CV Numeric array of observation CVs
#' @slot Units Character units of the index
#' @slot Ref Numeric vector length `nFleet` of reference values
#' @slot RefCV Numeric array of reference CVs
#' @slot Timing Numeric vector of observation timing
#' @slot Selectivity Selectivity mapping for the index
#' @slot Misc Additional metadata
#'
#' @include class-unions.R
#'
#' @export
setClass(
  "indicesdata",
  slots = c(
    Name        = "char.null",
    Value       = "array.null",
    CV          = "array.null",
    Units       = "char.null",
    Ref         = "num.null",
    RefCV       = "array.null",
    Timing      = "numeric",
    Selectivity = "array.char.num",
    Misc        = "list"
  )
)


#' Composition Data Class
#'
#' S4 class defining age or length composition observations.
#'
#' @slot Name Character name of the dataset
#' @slot Value Numeric array of composition proportions or counts
#' @slot Classes Numeric vector of class boundaries
#' @slot Units Character units of the data
#' @slot Log Optional log information
#' @slot Misc Additional metadata
#'
#' @include class-unions.R
#'
#' @export
setClass(
  "compdata",
  slots = c(
    Name    = "char.null",
    Value   = "array.null",
    Classes = "num.null",
    Units   = "char.null",
    Log     = "list",
    Misc    = "list"
  )
)


#' Life History Data Class
#'
#' S4 class grouping biological life-history components.
#'
#' @slot Ages Age structure definition
#' @slot Length Length-at-age model
#' @slot Weight Weight-at-age model
#' @slot NaturalMortality Natural mortality model
#' @slot Maturity Maturity-at-age model
#' @slot Fecundity Fecundity model
#' @slot SRR Stock–recruit relationship
#' @slot Spatial Spatial structure
#' @slot Depletion Initial depletion specification
#' @slot Misc Additional metadata
#'
#' @include class-unions.R
#' @include class-stock.R
#'
#' @export
setClass(
  "lifehistorydata",
  slots = c(
    Ages              = "ages",
    Length            = "length",
    Weight            = "weight",
    NaturalMortality  = "naturalmortality",
    Maturity          = "maturity",
    Fecundity         = "fecundity",
    SRR               = "srr",
    Spatial           = "spatial",
    Depletion         = "depletion",
    Misc              = "list"
  )
)


#' Exploitation Data Class
#'
#' S4 class defining fleet-specific exploitation processes.
#'
#' @slot Selectivity Fishing selectivity
#' @slot Retention Retention-at-age or length
#' @slot DiscardMortality Discard mortality model
#' @slot Misc Additional metadata
#'
#' @include class-unions.R
#' @include class-selectivity.R
#' @include class-retention.R
#' @include class-discardmortality.R
#'
#' @export
setClass(
  "exploitationdata",
  slots = c(
    Selectivity      = "selectivity",
    Retention        = "retention",
    DiscardMortality = "discardmortality",
    Misc             = "list"
  )
)


#' Reference Data Class
#'
#' S4 class containing biological or management reference points.
#'
#' @slot Misc Additional metadata
#'
#' @include class-internal.R
#'
#' @export
setClass(
  "referencedata",
  slots = c(
    Misc = "list"
  ),
  contains = "refpointsMSY"
)


#' Advice Data Class
#'
#' S4 class defining management advice outputs.
#'
#' @slot TAC Numeric array of total allowable catch
#' @slot Effort Numeric array of advised effort
#' @slot Misc Additional metadata
#'
#' @include class-unions.R
#'
#' @export
setClass(
  "advicedata",
  slots = c(
    TAC    = "num.array.null",
    Effort = "num.array.null",
    Misc   = "list"
  )
)
