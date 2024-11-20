assignSlot <- function(x, value, slot) {
  nms <- slotNames(x)
  if (!slot %in% nms) {
    cli::cli_alert_warning('Slot {.code {slot}} not found in object class: {.code {class(x)}}')
    return()
  }
  slot(x, slot) <- value
  if ('Modified' %in% nms)
    slot(x, 'Modified') <- Sys.time()
  methods::validObject(x)
  x
}


#' Access and Assign Slots for New Style S4 Class Objects
#'
#' @param x A new style S4 Class Object such as [OM()], [Stock()], or [Fleet()]
#' that includes a slot corresponding to the function name
#' @param value A value to assign to the slot corresponding to the function name
#' in object `x`
#'
#' @name Access
#'
NULL

## ---- AC ----

#' @rdname Access
#' @export
AC <- function(x) {
  x@AC
}

#' @rdname Access
#' @export
`AC<-` <- function(x, value) {
  assignSlot(x, value, 'AC')
}


## ---- ApicalF ----

#' @rdname Access
#' @export
ApicalF <- function(x) {
  x@ApicalF
}

#' @rdname Access
#' @export
`ApicalF<-` <- function(x, value) {
  assignSlot(x, value, 'ApicalF')
}


## ---- Agency ----

#' @rdname Access
#' @export
Agency <- function(x) {
  x@Agency
}

#' @rdname Access
#' @export
`Agency<-` <- function(x, value) {
  assignSlot(x, value, 'Agency')
}

## ---- Author ----

#' @rdname Access
#' @export
Author <- function(x) {
  x@Author
}

#' @rdname Access
#' @export
`Author<-` <- function(x, value) {
  assignSlot(x, value, 'Author')
}

## ---- ASK ----

#' @rdname Access
#' @export
ASK <- function(x) {
  x@ASK
}

#' @rdname Access
#' @export
`ASK<-` <- function(x, value) {
  assignSlot(x, value, 'ASK')
}

## ---- Catchability ----

#' @rdname Access
#' @export
Catchability <- function(x) {
  x@Catchability
}

#' @rdname Access
#' @export
`Catchability<-` <- function(x, value) {
  assignSlot(x, value, 'Catchability')
}


## ---- Classes ----

#' @rdname Access
#' @export
Classes <- function(x) {
  x@Classes
}

#' @rdname Access
#' @export
`Classes<-` <- function(x, value) {
  assignSlot(x, value, 'Classes')
}

## ----CommonName ----

#' @rdname Access
#' @export
CommonName <- function(x) {
  x@CommonName
}

#' @rdname Access
#' @export
`CommonName<-` <- function(x, value) {
  assignSlot(x, value, 'CommonName')
}


## ---- Complexes ----

#' @rdname Access
#' @export
Complexes <- function(x) {
  x@Complexes
}

#' @rdname Access
#' @export
`Complexes<-` <- function(x, value) {
  assignSlot(x, value, 'Complexes')
}

## ---- Control ----

#' @rdname Access
#' @export
Control <- function(x) {
  x@Control
}

#' @rdname Access
#' @export
`Control<-` <- function(x, value) {
  assignSlot(x, value, 'Control')
}

## ---- Created ----

#' @rdname Access
#' @export
Created <- function(x) {
  x@Created
}

## ---- CurrentYear ----

#' @rdname Access
#' @export
CurrentYear <- function(x) {
  x@CurrentYear
}

#' @rdname Access
#' @export
`CurrentYear<-` <- function(x, value) {
  assignSlot(x, value, 'CurrentYear')
}


## ---- CVatAge ----

#' @rdname Access
#' @export
CVatAge <- function(x) {
  x@CVatAge
}

#' @rdname Access
#' @export
`CVatAge<-` <- function(x, value) {
  assignSlot(x, value, 'CVatAge')
}


## ---- CVDist ----

#' @rdname Access
#' @export
CVDist <- function(x) {
  x@CVDist
}

#' @rdname Access
#' @export
`CVDist<-` <- function(x, value) {
  assignSlot(x, value, 'CVDist')
}

## ---- CVStay ----

#' @rdname Access
#' @export
CVStay <- function(x) {
  x@CVStay
}

#' @rdname Access
#' @export
`CVStay<-` <- function(x, value) {
  assignSlot(x, value, 'CVStay')
}


## ---- Email ----

#' @rdname Access
#' @export
Email <- function(x) {
  x@Email
}

#' @rdname Access
#' @export
`Email<-` <- function(x, value) {
  assignSlot(x, value, 'Email')
}

## ---- Final ----

#' @rdname Access
#' @export
Final <- function(x) {
  x@Final
}

#' @rdname Access
#' @export
`Final<-` <- function(x, value) {
  assignSlot(x, value, 'Final')
}


## ---- FracOther ----

#' @rdname Access
#' @export
FracOther <- function(x) {
  x@FracOther
}

#' @rdname Access
#' @export
`FracOther<-` <- function(x, value) {
  assignSlot(x, value, 'FracOther')
}



## ---- Initial ----

#' @rdname Access
#' @export
Initial <- function(x) {
  x@Initial
}

#' @rdname Access
#' @export
`Initial<-` <- function(x, value) {
  assignSlot(x, value, 'Initial')
}


## ---- Interval ----

#' @rdname Access
#' @export
Interval <- function(x) {
  x@Interval
}

#' @rdname Access
#' @export
`Interval<-` <- function(x, value) {
  assignSlot(x, value, 'Interval')
}


## ---- Latitude ----

#' @rdname Access
#' @export
Latitude <- function(x) {
  x@Latitude
}

#' @rdname Access
#' @export
`Latitude<-` <- function(x, value) {
  assignSlot(x, value, 'Latitude')
}

## ---- Longitude ----

#' @rdname Access
#' @export
Longitude <- function(x) {
  x@Longitude
}

#' @rdname Access
#' @export
`Longitude<-` <- function(x, value) {
  assignSlot(x, value, 'Longitude')
}

## nAge ----

#' @rdname Access
#' @export
nAge <- function(x) {
  if (methods::is(x, 'stock'))
    return(x@Ages@MaxAge+1)
  if (methods::is(x, 'ages'))
    return(x@MaxAge+1)
}

## nTS ----

#' @rdname Access
#' @export
nTS <- function(x) {
  TimeSteps(x) |> length()
}

## nArea ----

#' @rdname Access
#' @export
nArea <- function(x) {
  dd <- dim(x@Spatial@UnfishedDist)
  if (length(dd)<1)
    return(1)
  dd[2]
}


## MaxAge ----


#' @rdname Access
#' @export
MaxAge <- function(x) {
  if (methods::is(x, 'stock'))
    return(x@Ages@MaxAge)
  x@MaxAge
}

#' @rdname Access
#' @export
`MaxAge<-` <- function(x, value) {
  if (value%%1!=0)
    cli::cli_abort('`value` must be an integer')
  x <- assignSlot(x, value, 'MaxAge')
  x@Classes <- 0:value
  x
}

## MeanAtAge ----

#' @rdname Access
#' @export
MeanAtAge <- function(x) {
  x@MeanAtAge
}

#' @rdname Access
#' @export
`MeanAtAge<-` <- function(x, value) {
  assignSlot(x, value, 'MeanAtAge')
}

## MeanAtLength ----

#' @rdname Access
#' @export
MeanAtLength <- function(x) {
  x@MeanAtLength
}

#' @rdname Access
#' @export
`MeanAtLength<-` <- function(x, value) {
  assignSlot(x, value, 'MeanAtLength')
}


## ---- Misc ----

#' @rdname Access
#' @export
Misc <- function(x) {
  x@Misc
}

#' @rdname Access
#' @export
`Misc<-` <- function(x, value) {
  assignSlot(x, value, 'Misc')
}


## ---- Modified ----

#' @rdname Access
#' @export
Modified <- function(x) {
  x@Modified
}

#' @rdname Access
#' @export
`Modified<-` <- function(x, value) {
  assignSlot(x, value, 'Modified')
}

## ---- Model ----

#' @rdname Access
#' @export
Model <- function(x) {
  x@Model
}

#' @rdname Access
#' @export
`Model<-` <- function(x, value) {
  assignSlot(x, value, 'Model')
}

## ---- Movement ----

#' @rdname Access
#' @export
Movement <- function(x) {
  x@Movement
}

#' @rdname Access
#' @export
`Movement<-` <- function(x, value) {
  assignSlot(x, value, 'Movement')
}


## ---- Name ----

#' @rdname Access
#' @export
Name <- function(x) {
  x@Name
}

#' @rdname Access
#' @export
`Name<-` <- function(x, value) {
  assignSlot(x, value, 'Name')
}


## ---- nSim ----

#' @rdname Access
#' @export
nSim <- function(x) {
  x@nSim
}

#' @rdname Access
#' @export
`nSim<-` <- function(x, value) {
  assignSlot(x, value, 'nSim')
}

#' @rdname Access
#' @export
nsim <- function(x) {
  x@nSim
}

#' @rdname Access
#' @export
`nsim<-` <- function(x, value) {
  assignSlot(x, value, 'nSim')
}

## ---- nReps ----

#' @rdname Access
#' @export
nReps <- function(x) {
  x@nReps
}

#' @rdname Access
#' @export
`nReps<-` <- function(x, value) {
  assignSlot(x, value, 'nReps')
}


## ---- nYears ----

#' @rdname Access
#' @export
nYears <- function(x) {
  x@nYears
}

#' @rdname Access
#' @export
`nYears<-` <- function(x, value) {
  assignSlot(x, value, 'nYears')
}

#' @rdname Access
#' @export
nyears <- function(x) {
  x@nYears
}

#' @rdname Access
#' @export
`nyears<-` <- function(x, value) {
  assignSlot(x, value, 'nYears')
}

## ---- Pars ----

#' @rdname Access
#' @export
Pars <- function(x) {
  x@Pars
}

#' @rdname Access
#' @export
`Pars<-` <- function(x, value) {
  assignSlot(x, value, 'Pars')
}

## ---- Period ----

#' @rdname Access
#' @export
Period <- function(x) {
  x@Period
}

#' @rdname Access
#' @export
`Period<-` <- function(x, value) {
  assignSlot(x, value, 'Period')
}

## ---- PlusGroup ----

#' @rdname Access
#' @export
PlusGroup <- function(x) {
  x@PlusGroup
}

#' @rdname Access
#' @export
`PlusGroup<-` <- function(x, value) {
  assignSlot(x, value, 'PlusGroup')
}

## ---- ProbStaying ----

#' @rdname Access
#' @export
ProbStaying <- function(x) {
  x@ProbStaying
}

#' @rdname Access
#' @export
`ProbStaying<-` <- function(x, value) {
  assignSlot(x, value, 'ProbStaying')
}


## ---- pStar ----

#' @rdname Access
#' @export
pStar <- function(x) {
  x@pStar
}

#' @rdname Access
#' @export
`pStar<-` <- function(x, value) {
  assignSlot(x, value, 'pStar')
}

## ---- pYears ----

#' @rdname Access
#' @export
pYears <- function(x) {
  x@pYears
}

#' @rdname Access
#' @export
`pYears<-` <- function(x, value) {
  assignSlot(x, value, 'pYears')
}


#' @rdname Access
#' @export
proyears <- function(x) {
  x@pYears
}

#' @rdname Access
#' @export
`proyears<-` <- function(x, value) {
  assignSlot(x, value, 'pYears')
}


## ---- RecDevInit ----

#' @rdname Access
#' @export
RecDevInit <- function(x) {
  x@RecDevInit
}

#' @rdname Access
#' @export
`RecDevInit<-` <- function(x, value) {
  assignSlot(x, value, 'RecDevInit')
}

## ---- RecDevHist ----

#' @rdname Access
#' @export
RecDevHist <- function(x) {
  x@RecDevHist
}

#' @rdname Access
#' @export
`RecDevHist<-` <- function(x, value) {
  assignSlot(x, value, 'RecDevHist')
}

## ---- RecDevProj ----

#' @rdname Access
#' @export
RecDevProj <- function(x) {
  x@RecDevProj
}

#' @rdname Access
#' @export
`RecDevProj<-` <- function(x, value) {
  assignSlot(x, value, 'RecDevProj')
}

## ---- Reference ----

#' @rdname Access
#' @export
Reference <- function(x) {
  x@Reference
}

#' @rdname Access
#' @export
`Reference<-` <- function(x, value) {
  assignSlot(x, value, 'Reference')
}


## ---- Region ----

#' @rdname Access
#' @export
Region <- function(x) {
  x@Region
}

#' @rdname Access
#' @export
`Region<-` <- function(x, value) {
  assignSlot(x, value, 'Region')
}


## ---- Relations ----

#' @rdname Access
#' @export
Relations <- function(x) {
  x@Relations
}

#' @rdname Access
#' @export
`Relations<-` <- function(x, value) {
  assignSlot(x, value, 'Relations')
}

## ---- RelativeSize ----

#' @rdname Access
#' @export
RelativeSize <- function(x) {
  x@RelativeSize
}

#' @rdname Access
#' @export
`RelativeSize<-` <- function(x, value) {
  assignSlot(x, value, 'RelativeSize')
}


## ---- SD ----

#' @rdname Access
#' @export
SD <- function(x) {
  x@SD
}

#' @rdname Access
#' @export
`SD<-` <- function(x, value) {
  assignSlot(x, value, 'SD')
}

## ---- SexPars ----

#' @rdname Access
#' @export
SexPars <- function(x) {
  x@SexPars
}

#' @rdname Access
#' @export
`SexPars<-` <- function(x, value) {
  assignSlot(x, value, 'SexPars')
}

## ---- Source ----

#' @rdname Access
#' @export
Source <- function(x) {
  x@Source
}

#' @rdname Access
#' @export
`Source<-` <- function(x, value) {
  assignSlot(x, value, 'Source')
}

## ---- Sponsor ----

#' @rdname Access
#' @export
Sponsor <- function(x) {
  x@Sponsor
}

#' @rdname Access
#' @export
`Sponsor<-` <- function(x, value) {
  assignSlot(x, value, 'Sponsor')
}

## ---- Seed ----

#' @rdname Access
#' @export
Seed <- function(x) {
  x@Seed
}

#' @rdname Access
#' @export
`Seed<-` <- function(x, value) {
  assignSlot(x, value, 'Seed')
}

## SpawnTimeFrac ----

#' @rdname Access
#' @export
SpawnTimeFrac <- function(x) {
  x@SpawnTimeFrac
}

#' @rdname Access
#' @export
`SpawnTimeFrac<-` <- function(x, value) {
  assignSlot(x, value, 'SpawnTimeFrac')
}

## Species ----

#' @rdname Access
#' @export
Species <- function(x) {
  x@Species
}

#' @rdname Access
#' @export
`Species<-` <- function(x, value) {
  assignSlot(x, value, 'Species')
}

## ---- TimeSteps ----

#' @rdname Access
#' @export
TimeSteps <- function(x, Period=NULL) {
  TimeSteps <- x@TimeSteps
  if (is.null(Period))
    return(TimeSteps)
  CalcTimeSteps(x@nYears, x@pYears, x@CurrentYear, x@TimeUnits, Period)
}

#' @rdname Access
#' @export
`TimeSteps<-` <- function(x, value) {
  assignSlot(x, value, 'TimeSteps')
}

## ---- TimeUnits ----

#' @rdname Access
#' @export
TimeUnits <- function(x, Period=NULL) {
 x@TimeUnits

}

#' @rdname Access
#' @export
`TimeUnits<-` <- function(x, value) {
  assignSlot(x, value, 'TimeUnits')
}


## ---- TruncSD ----

#' @rdname Access
#' @export
TruncSD <- function(x) {
  x@TruncSD
}

#' @rdname Access
#' @export
`TruncSD<-` <- function(x, value) {
  assignSlot(x, value, 'TruncSD')
}


## ---- Units ----

#' @rdname Access
#' @export
Units <- function(x) {
  x@Units
}

#' @rdname Access
#' @export
`Units<-` <- function(x, value) {
  assignSlot(x, value, 'Units')
}

## ---- UnfishedDist ----

#' @rdname Access
#' @export
UnfishedDist <- function(x) {
  x@UnfishedDist
}

#' @rdname Access
#' @export
`UnfishedDist<-` <- function(x, value) {
  assignSlot(x, value, 'UnfishedDist')
}



