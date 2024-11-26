# ---- Parameters & Slots ----
AgeClasses_param <- function() {
  "Numeric vector length `MaxAge+1`. The age classes for the stock. Always starts at 0. Populated internally from `MaxAge`."
}

ASK_param <- function(type='length') {
  paste0('The age-', type, ' key. Calculated internally from `MeanAtAge` and `CVatAge`.
         The `Classes` slot is populated internally unless specified in the object.')
}


Created_param <- function() {
  'Time the object was created.'
}

Classes_param <- function(type='length') {
  paste0('The ', type, ' classes used for the calculation of the age-size key (`ASK`).
         Will also be used for calculating at-', type, ' schedules, e.g., [Maturity()] or [Selectivity()].')
}

CVatAge_param <- function(name='Length') {
    paste0('Coefficient of variability for the ', name, '-at-Age schedule.
    Should be a 3 dimensional array, with the first dimension either length 1 or
    length `nSim`, the second dimension length `nAge`, and the third dimension either length 1 or length `nTS`.
    Alternatively, can be a single numeric value, which applies to all age classes and time steps.
           ')
}



Dist_param <- function() {
  "Character. Length 1. Either `normal` or `lognormal`. Used to generate size composition using `MeanAtAge`, `SDatAge`, and `TruncSD`."
}

MaxAge_param <- function() {
  "An integer value (length 1) specifying the maximum age to include in the model. If `PlusGroup==TRUE`, `MaxAge` will be a plusgroup. **Required**. Note that the age classes always start at 0, so there will be `MaxAge+1` age classes."
}


MeanAtAge_param <- function(name='Fecundity') {
  paste0(name, '-at-Age schedule.  Calculated internally from `Pars` and `Model`. If populated manually,
  `Pars` and `Model` will be ignored. Should be a 3 dimensional array, with the first dimension either length 1 or
         length `nSim`, the second dimension length `nAge`, and the third dimension either length 1
         or length `nTS`. ')
}

MeanAtLength_param <- function(name='Fecundity') {
  paste0(name, '-at-Length schedule.  Calculated internally from `Pars` and `Model`. If populated manually,
  `Pars` and `Model` will be ignored. Should be a 3 dimensional array, with the first dimension either length 1 or
         length `nSim`, the second dimension length `nClasses`, and the third dimension either length 1
         or length `nTS`. ')
}

Misc_param <- function() {
  "A list for additional miscellaneous objects. Not currently used."
}

Model_param <- function(type='Length') {
  paste0(" A character string or a R function. If `Pars` corresponds to a built-in model (see [",
         type,
         "Models()]), then `Model` can be left blank and will be populated internally.
         Alternatively, `Model` can be a R function that takes the arguments named in `Pars`.")
}

Modified_param <- function() {
  'Time the object was last modified.'
}

Pars_param <- function() {
  'A named list of parameters for a model to generate `MeanAtAge` or `MeanAtLength`. See `Parameters` section in `Details`.'
}


Plusgroup_param <- function() {
  'Include a plus-group? Defaults to TRUE.'
}

Random_param <- function() {
  "An array with dimensions matching `MeanAtAge`. `MeanAtAge` is assumed to represent
  the mean value for each age class in each time step, and is used to calculate the reference points.
  `Random` is additional variability that is NOT used when calculating reference points.
  NOTE: `Random` is not currently implemented."
}

Timing_param <- function() {
  "Numeric value between 0 and 1. The relative time within a time step, where 0
  means the beginning of the time step and 1 the end of the time step.
  Defaults to 0."
}

TruncSD_param <- function() {
 'The number of standard deviations to truncate the distribution used to create `ASK`.
  Defaults to 2. Use a large number for a non-truncated distribution.'
}

Units_param <- function(variable='MaxAge', class='Ages', default='year') {
  paste0('A character string describing the units corresponding to `', variable,
         '`. Valid values are found in `ValidUnits(', class, '())`. Defaults to `',
         default, '` if not specified.')
}


# ---- Details ----

Pars_details <- function() {
  '
  `Pars` is a named list

 '
}

# ---- See Also ----

See_Also <- function(class, other=NULL) {
 slots <- slotNames(class)
 txt <- paste(paste0('[', slots, '()]'), collapse=', ')
 if (!is.null(other)) {
   other <- paste0('[', other, '()]', collapse=', ')
   txt <- paste(txt, other, sep=', ')
 }
 txt
}

Creating_New_Objects <- function(class) {

  if (class %in% c('stock', 'fleet', 'obs', 'imp', 'om'))
    return(
      paste0("New `", class, "` objects can be created with `",
           paste0(firstup(class), "()"), "`, see `Usage` and `Examples` for details.")
    )

  slots <- c(slotNames('stock'), slotNames('fleet'))
  ind <- match(class, tolower(slots))

  paste0("New `", class, "` objects can be created with `",
                 paste0(slots[ind], "()"), "`, see `Usage` and `Examples` for details.")
}

Accessing_Assigning_Slots <- function(class) {
  paste0("Like all of the new style objects in `MSEtool`, the slots in `",
         class, "` objects can be accessed and assigned with functions matching the slot names. See `See Also` section for functions.")
}




