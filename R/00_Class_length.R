
#' Length Class and Constructor
#' 
#' The `Length` class defines the length-at-age structure associated with a
#' [Stock()] objectect. It stores the parameters and model used to generate
#' mean length-at-age, along with variability and assumptions regarding the
#' distribution of length-at-age.
#' 
#' @param Pars A named list of parameters defining a valid length-at-age model
#'   (see [LengthModels()]). Not required if `MeanAtAge` is supplied directly.
#' @param Model A character string or function identifying the growth model.
#'   If `NULL`, the model is inferred from `Pars`. Not required if `MeanAtAge` is supplied directly.
#' @param Units Character string giving the length units (e.g. `"mm"`).
#' @param MeanAtAge Numeric array giving mean length-at-age directly.
#' @param CVatAge Numeric value or array giving the coefficient of variation
#'   of length-at-age.
#' @param Dist Character string specifying the error distribution.
#' @param TruncSD Numeric value or array specifying truncation in SD units.
#' @param Timing Numeric value or array specifying timing within the time step.
#' @param Random Optional random effects structure. Not currently used. 
#' @param Classes Optional numeric vector of length classes used to for `at-length` 
#' schedueles (e.g., [Maturity()], [Selectivity()].
#' @param Misc A list for additional miscellaneous objectects.
#' @param x A [Stock()] objectect.
#' @param value A [Length()] objectect to assign.
#' 
#' @details
#' Note: `Pars` will overwrite `MeanAtAge`
#' 
#' Named dimensions 
#' 
#' #' @details
#'
#' The `Length` generic is used to:
#' * construct new `Length` objectects;
#' * access `Length` when supplied with a [Stock()] objectect;
#' * assign a `Length` objectect to a [Stock()] objectect.
#'
#' ## Parameters (`Pars`)
#'
#' The `Pars` slot stores parameters for the growth model used to generate
#' mean length-at-age. Supported structures include:
#'
#' * **Constant**: numeric scalar
#' * **Uniform across simulations**: numeric length 2
#' * **Lognormal inter-annual variation**: `SD`-suffixed parameters
#' * **Simulation-specific**: numeric vector of length `nSim`
#' * **Time-varying**: numeric matrix with `nTS` columns
#'
#' ## Slots
#'
#' Objects of class `"length"` contain the following slots:
#'
#' * `Pars`: Named list of growth parameters
#' * `Model`: Growth model identifier
#' * `Units`: Length units
#' * `MeanAtAge`: Mean length-at-age array
#' * `CVatAge`: Coefficient of variation at age
#' * `Dist`: Distribution name
#' * `TruncSD`: Truncation in SD units
#' * `Timing`: Timing within time step
#' * `Random`: Random effects
#' * `ASK`: Age–length key
#' * `Classes`: Length classes
#' * `Misc`: Additional metadata
#'
#' @return
#' * `Length`: a [Length] class object
#' * `Length(x)`: a `Length` object from object `x`
#' * `Length<-`: the modified [Stock()] object
#'
#' @seealso [LengthModels()], [Populate()], [Stock()]
#'
#' @name Length
#' @rdname Length
#'
#' @example man-examples/class-length.R
#' 
#'
#' @include 00_Class_unions.R
NULL

setClass("length",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null',
                 MeanAtAge='num.array.null',
                 CVatAge='num.array.null',
                 Dist='character',
                 TruncSD='num.array.null',
                 Timing='num.array.null',
                 Random='num.array.null',
                 ASK='array.null',
                 Classes='num.null',
                 Misc='list'
         )
)

setValidity("length", function(object) {
  
  # TODO - update for all legitimate cases
  
  # if (!is.list(objectect@Pars))
  #   return("Pars must be a list")
  # 
  # if (!is.null(objectect@MeanAtAge) && !is.numeric(objectect@MeanAtAge))
  #   return("MeanAtAge must be numeric or NULL")
  # 
  # if (length(objectect@Units) > 1)
  #   return("Units must be length 1 or NULL")
  
  TRUE
})


setMethod("initialize", "length", function(.Object,
                                           Pars=list(Linf=NA, K=NA, t0=NA),
                                           Model=NULL,
                                           Units='mm',
                                           MeanAtAge=NULL,
                                           CVatAge=0.1,
                                           Dist='normal',
                                           TruncSD=2,
                                           Timing=0,
                                           Random=NULL,
                                           ASK=NULL,
                                           Classes=NULL,
                                           Misc=list()) {
  .Object@Pars <- Pars
  .Object@Units <- Units
  .Object@MeanAtAge <- MeanAtAge
  .Object@CVatAge <- CVatAge
  .Object@Dist <- Dist
  .Object@TruncSD <- TruncSD
  .Object@Timing <- Timing
  .Object@Random <- Random
  .Object@ASK <- ASK
  .Object@Classes <- Classes
  .Object@Misc <- Misc

  if (!is.null(Model))
    .Object@Model <- Model

  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    .Object@Model <- FindModel(.Object)

  .Object
})
