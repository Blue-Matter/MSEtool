# help topic documenting the accepted inputs of each class
.InputHelpTopic <- c(
  om               = 'OM',
  stock            = 'Stock',
  fleet            = 'Fleet',
  obs              = 'Obs',
  ages             = 'Ages',
  length           = 'Length',
  weight           = 'Weight',
  naturalmortality = 'NaturalMortality',
  maturity         = 'Maturity',
  fecundity        = 'Fecundity',
  srr              = 'SRR',
  spatial          = 'Spatial',
  depletion        = 'Depletion',
  effort           = 'Effort',
  catchability     = 'Catchability',
  selectivity      = 'Selectivity',
  retention        = 'Retention',
  discardmortality = 'DiscardMortality',
  effortobs        = 'EffortObs',
  catchobs         = 'CatchObs',
  indicesobs       = 'IndicesObs',
  compobs          = 'CompObs',
  imp              = 'Imp',
  impslot          = 'ImpSlot'
)

# help topic listing the models available for each class with a `Model` slot
.ModelHelpTopic <- c(
  length           = 'LengthModels',
  weight           = 'WeightModels',
  naturalmortality = 'NaturalMortalityModels',
  maturity         = 'MaturityModels',
  fecundity        = 'FecundityModels',
  srr              = 'SRRModels',
  selectivity      = 'SelectivityModels',
  retention        = 'RetentionModels'
)

# abort listing every invalid input of `object`, with a link to its help page
.CheckInputs <- function(object, label = NULL) {
  Problems <- .InputProblems(object)
  if (length(Problems))
    .AbortInvalidInputs(Problems, class(object)[1], label)
  invisible(object)
}

# `where` accumulates the labels added as the error passes up through .SafePopulate()
.AbortInvalidInputs <- function(Problems, cl, where = NULL, stage = 'input') {
  Topic  <- if (cl %in% names(.InputHelpTopic)) .InputHelpTopic[[cl]] else "Populate"
  Prefix <- if (length(where)) paste0(paste(where, collapse = ' > '), ': ') else ''
  if (stage == 'input') {
    Header <- paste0(Prefix, if (nzchar(Prefix)) 'invalid' else 'Invalid',
                     if (length(Problems) > 1) ' inputs' else ' input',
                     cli::format_inline(" in {.cls {cl}} object."))
    Help   <- cli::format_inline("See {.help MSEtool::{Topic}} for the accepted values.")
  } else {
    Header <- paste0(Prefix, if (nzchar(Prefix)) 'populated' else 'Populated',
                     cli::format_inline(" {.cls {cl}} object has the wrong dimensions."))
    Help   <- cli::format_inline("Check the dimensions of any arrays supplied directly to this object. See {.help MSEtool::{Topic}}.")
  }
  cli::cli_abort(
    c("x" = .EscapeBraces(Header),
      stats::setNames(.EscapeBraces(Problems), rep("*", length(Problems))),
      "i" = .EscapeBraces(Help)),
    call         = NULL,
    class        = 'MSEtool_invalid_input',
    problems     = Problems,
    object_class = cl,
    where        = where,
    stage        = stage
  )
}

# structure of a populated object: every array has named dimensions that agree with the OM
.CheckOutput <- function(object, nSim = NULL, Ages = NULL, Years = NULL, nArea = NULL, label = NULL) {
  Problems <- .OutputProblems(object, nSim, Ages, Years, nArea)
  if (length(Problems))
    .AbortInvalidInputs(Problems, class(object)[1], label, stage = 'output')
  invisible(object)
}

# dimensions, in order, of each array slot after population (`Pars` applies to every element)
.ExpectedDims <- local({
  S     <- 'Sim'
  SY    <- c('Sim', 'Year')
  SAY   <- c('Sim', 'Age', 'Year')
  SCY   <- c('Sim', 'Class', 'Year')
  SACY  <- c('Sim', 'Age', 'Class', 'Year')
  SYA   <- c('Sim', 'Year', 'Area')
  SAYA  <- c('Sim', 'Age', 'Year', 'Area')
  SCYA  <- c('Sim', 'Class', 'Year', 'Area')
  SAAY  <- c('Sim', 'Area', 'Age', 'Year')
  SFTAY <- c('Sim', 'FromArea', 'ToArea', 'Age', 'Year')

  AtSize <- list(
    MeanAtAge    = SAY,
    MeanAtLength = SCY,
    MeanAtWeight = SCY,
    Pars         = SY
  )

  FleetAtSize <- list(
    MeanAtAge    = SAYA,
    MeanAtLength = SCYA,
    MeanAtWeight = SCYA,
    Pars         = SYA
  )

  list(
    length = list(
      MeanAtAge = SAY,
      CVatAge   = SAY,
      ALK       = SACY,
      Pars      = SY
    ),
    weight = list(
      MeanAtAge    = SAY,
      MeanAtLength = SCY,
      CVatAge      = SAY,
      AWK          = SACY,
      Pars         = SY
    ),
    naturalmortality = AtSize,
    maturity         = c(AtSize, list(Semelparous = SAY)),
    fecundity        = AtSize,
    srr = list(
      R0         = SY,
      SD         = SY,
      AC         = SY,
      RecDevInit = c('Sim', 'Age'),
      RecDevHist = SY,
      RecDevProj = SY,
      Pars       = SY
    ),
    spatial = list(
      UnfishedDist = SAAY,
      ProbStaying  = SAAY,
      Movement     = SFTAY,
      FracOther    = SFTAY,
      RelativeSize = c('Sim', 'Area')
    ),
    depletion = list(
      Initial = S,
      Final   = S
    ),
    selectivity      = FleetAtSize,
    retention        = FleetAtSize,
    discardmortality = list(
      MeanAtAge    = SAYA,
      MeanAtLength = SCYA
    ),
    effort = list(
      Effort               = SY,
      Distribution         = SYA,
      Targeting            = SY,
      ReleaseValue         = SY,
      Maximum              = SY,
      TripsScalar          = SY,
      AnglerPerTrip        = SY,
      Theta                = SY,
      StockTargetingLambda = SY
    ),
    catchability = list(
      Efficiency = SY
    ),
    fleet = list(
      Closure             = SYA,
      WeightFleetSelected = SAY,
      WeightFleetRetained = SAY
    ),
    effortobs = list(
      CV    = S,
      Bias  = S,
      Error = SY,
      Ref   = S
    ),
    catchobs = list(
      CV    = S,
      Bias  = S,
      Error = SY,
      Ref   = S
    ),
    indicesobs = list(
      CV    = S,
      Error = SY,
      Beta  = S,
      AC    = S,
      Ref   = S,
      qInc  = S
    ),
    compobs = list(
      SampleSize = SY,
      ESS        = SY,
      Theta      = SY
    ),
    impslot = list(
      Mean       = S,
      SD         = S,
      Compliance = SY,
      Error      = SY
    )
  )
})

# an unnamed array is labelled by position: each dimension must have length 1 or one of the expected sizes (NA = any)
.CheckUnnamedDims <- function(dd, name, cl, Expected) {
  Shape    <- paste(names(Expected), collapse = ' x ')
  Problems <- if (length(dd) != length(Expected)) {
    paste0('`', name, '` must be a ', Shape, ' array; found ', length(dd), ' dimension(s).')
  } else {
    unlist(purrr::imap(Expected, \(n, d) {
      k <- dd[match(d, names(Expected))]
      if (anyNA(n) || k %in% c(1, n))
        return(NULL)
      Sizes <- unique(c(1, n))
      if (d == 'Year')
        return(paste0('`', name, '` has ', k, ' values in its unnamed Year dimension; must have ',
                      paste(Sizes, collapse = ', '), ' (one, the historical, or all time steps), ',
                      'or name the Year dimension to give other years.'))
      paste0('`', name, '` has ', k, ' values in its ', d, ' dimension; must have ',
             paste(Sizes, collapse = ' or '), '.')
    }))
  }
  if (length(Problems))
    .AbortInvalidInputs(Problems, cl)
  invisible(TRUE)
}

.KnownDims <- c('Sim', 'Age', 'Year', 'Area', 'Class', 'FromArea', 'ToArea', 'Stock', 'Fleet')

.OutputProblems <- function(object, nSim = NULL, Ages = NULL, Years = NULL, nArea = NULL) {
  Skip  <- c('Misc', 'Log', 'Years', 'Classes', 'Name', if (methods::is(object, 'indicesobs')) 'Selectivity')
  Slots <- setdiff(methods::slotNames(object), Skip)
  Out   <- NULL
  for (sl in Slots) {
    Val <- methods::slot(object, sl)
    if (isS4(Val) || !length(Val))
      next
    Items <- if (is.list(Val) && !is.data.frame(Val)) {
      if (is.null(names(Val)))
        names(Val) <- seq_along(Val)
      stats::setNames(Val, paste0(sl, '$', names(Val)))
    } else {
      stats::setNames(list(Val), sl)
    }
    for (nm in names(Items))
      Out <- c(Out, .ArrayDimProblems(Items[[nm]], nm, sl, object, nSim, Ages, Years, nArea))
  }
  Out
}

.MatchesValues <- function(Names, Values) {
  Num <- suppressWarnings(as.numeric(Names))
  length(Num) == length(Values) && !anyNA(Num) && all(abs(Num - Values) < 1e-6)
}

.ArrayDimProblems <- function(x, name, slot, object, nSim, Ages, Years, nArea) {
  if (!is.array(x) || !length(x))
    return(NULL)
  DN       <- dimnames(x)
  DNames   <- names(DN)
  Expected <- .ExpectedDims[[class(object)[1]]][[slot]]
  Shape    <- paste(Expected %||% c('Sim', 'Year'), collapse = ' x ')
  if (is.null(DNames) || any(!nzchar(DNames)) || any(vapply(DN, is.null, logical(1))))
    return(paste0('`', name, '` must have named dimensions (', Shape, ').'))
  if (any(dim(x) < 1))
    return(paste0('`', name, '` has an empty dimension; every dimension must have length 1 or more.'))
  if (!is.null(Expected) && !identical(DNames, Expected))
    return(paste0('`', name, '` has dimensions ', paste(DNames, collapse = ' x '),
                  '; must be ', Shape, '.'))
  Bad <- setdiff(DNames, .KnownDims)
  if (length(Bad))
    return(paste0('`', name, '` has unknown dimension name(s) ', .FormatVals(Bad), '.'))

  Out <- NULL
  if (!is.null(nSim) && 'Sim' %in% DNames && !length(DN$Sim) %in% c(1, nSim))
    Out <- c(Out, paste0('`', name, '` has ', length(DN$Sim),
                         ' simulations; must have 1 or `nSim` (', nSim, ').'))
  if (!is.null(Ages) && 'Age' %in% DNames) {
    AgeClasses <- Ages@Classes
    if (identical(slot, 'RecDevInit'))
      AgeClasses <- AgeClasses[-1]
    if (length(DN$Age) != 1 && !.MatchesValues(DN$Age, AgeClasses))
      Out <- c(Out, paste0('`', name, '` has ', length(DN$Age), ' age classes (',
                           .FormatVals(DN$Age), '); must have 1 or match the ', length(AgeClasses),
                           ' age classes of the stock (', .FormatVals(AgeClasses), ').'))
  }
  if (!is.null(Years) && 'Year' %in% DNames) {
    Yr      <- suppressWarnings(as.numeric(DN$Year))
    Outside <- DN$Year[is.na(Yr) | vapply(Yr, function(y) !any(abs(y - Years) < 1e-6), logical(1))]
    if (length(Outside))
      Out <- c(Out, paste0('`', name, '` has years outside the model years (',
                           min(Years), '-', max(Years), '): ', .FormatVals(Outside), '.'))
  }
  if (!is.null(nArea))
    for (d in intersect(c('Area', 'FromArea', 'ToArea'), DNames))
      if (!length(DN[[d]]) %in% c(1, nArea))
        Out <- c(Out, paste0('`', name, '` has ', length(DN[[d]]), ' values in its ', d,
                             ' dimension; must have 1 or `nArea` (', nArea, ').'))
  if ('Class' %in% DNames && slot %in% c('ALK', 'AWK') &&
      methods::.hasSlot(object, 'Classes') && length(object@Classes) &&
      length(DN$Class) != 1 && !.MatchesValues(DN$Class, object@Classes))
    Out <- c(Out, paste0('The size classes of `', name, '` (', .FormatVals(DN$Class),
                         ') do not match `Classes` (', .FormatVals(object@Classes), ').'))
  Out
}

.EscapeBraces <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)

.InputProblems <- function(object) {
  cl       <- class(object)[1]
  Fn       <- get0(paste0('.InputProblems_', cl), envir = asNamespace('MSEtool'),
                   mode = 'function', inherits = FALSE)
  Explicit <- if (!is.null(Fn)) Fn(object)
  Validity <- .ValidityProblems(object)
  Validity <- Validity[!.FirstTick(Validity) %in% .FirstTick(Explicit)]
  unique(c(Explicit, Validity))
}

.FirstTick <- function(x) {
  if (!length(x))
    return(character())
  m <- regexpr("`[^`]+`", x)
  ifelse(m > 0, substring(x, m, m + attr(m, 'match.length') - 1), NA_character_)
}

# messages from the class validity method; one that errors is left to the explicit checks
.ValidityProblems <- function(object) {
  Fn <- methods::getValidity(methods::getClass(class(object)[1]))
  if (is.null(Fn))
    return(NULL)
  Out <- tryCatch(Fn(object), error = function(e) TRUE)
  if (isTRUE(Out)) NULL else as.character(Out)
}

.FormatVals <- function(x, n = 3) {
  x   <- unique(x)
  Out <- paste(utils::head(format(x, digits = 4, trim = TRUE), n), collapse = ', ')
  if (length(x) > n) paste0(Out, ', ...') else Out
}

.RangeText <- function(lower, upper, lowerOpen, upperOpen) {
  if (is.finite(lower) && is.finite(upper))
    return(paste0('in ', if (lowerOpen) '(' else '[', lower, ', ', upper, if (upperOpen) ')' else ']'))
  if (is.finite(lower))
    return(paste(if (lowerOpen) '>' else '>=', lower))
  paste(if (upperOpen) '<' else '<=', upper)
}

# NULL when valid, else a message; na = 'none', 'all' (all-NA means unset), or 'any'
.CheckNum <- function(x,
                      name,
                      lower = -Inf,
                      upper = Inf,
                      lowerOpen = FALSE,
                      upperOpen = FALSE,
                      na = c('none', 'all', 'any'),
                      len = NULL,
                      whole = FALSE) {
  na <- match.arg(na)
  if (is.null(x) || !length(x))
    return(NULL)
  if (is.list(x) && !is.data.frame(x))
    return(unlist(purrr::imap(x, \(el, i) .CheckNum(el, paste0(name, '[[', i, ']]'), lower, upper,
                                                    lowerOpen, upperOpen, na, len, whole))))
  if (is.data.frame(x))
    return(NULL)
  AllNA <- all(is.na(x))
  if (!is.numeric(x) && !(is.logical(x) && AllNA))
    return(paste0('`', name, '` must be numeric, not ', class(x)[1], '.'))
  if (AllNA && na != 'none')
    return(NULL)
  if (!is.null(len) && !length(x) %in% len)
    return(paste0('`', name, '` must have length ', paste(len, collapse = ' or '),
                  ', not ', length(x), '.'))
  if (anyNA(x) && na != 'any')
    return(paste0('`', name, '` contains `NA`',
                  if (na == 'all') ' (use `NULL` or all `NA` to leave it unset)', '.'))
  Vals <- x[!is.na(x)]
  if (any(!is.finite(Vals)))
    return(paste0('`', name, '` must be finite.'))
  Tol <- 1e-8 * pmax(1, abs(Vals))
  Bad <- Vals[(Vals < lower - Tol) | (lowerOpen & abs(Vals - lower) <= Tol) |
                (Vals > upper + Tol) | (upperOpen & abs(Vals - upper) <= Tol)]
  if (length(Bad))
    return(paste0('`', name, '` must be ', .RangeText(lower, upper, lowerOpen, upperOpen),
                  '; found ', .FormatVals(Bad), '.'))
  NotWhole <- abs(Vals - round(Vals)) > 1e-8
  if (whole && any(NotWhole))
    return(paste0('`', name, '` must be a whole number; found ', .FormatVals(Vals[NotWhole]), '.'))
  NULL
}

.CheckChoice <- function(x, name, options, help = NULL, required = FALSE) {
  if (is.null(x) || (!length(x) && !required) || (!required && identical(x, '')))
    return(NULL)
  Options <- paste0('"', options, '"', collapse = ', ')
  if (!is.character(x) || length(x) != 1 || is.na(x))
    return(paste0('`', name, '` must be a single character string: one of ', Options, '.'))
  if (x %in% options)
    return(NULL)
  Closest <- .ClosestName(x, options)
  paste0('`', name, '` is "', x, '"; must be one of ', Options, '.',
         if (!is.null(Closest)) paste0(' Did you mean "', Closest, '"?'),
         if (!is.null(help)) paste0(' See ', help, '.'))
}

.CheckFlag <- function(x, name) {
  if (is.null(x) || !length(x))
    return(NULL)
  if (!is.logical(x) || length(x) != 1 || is.na(x))
    return(paste0('`', name, '` must be `TRUE` or `FALSE`.'))
  NULL
}

.CheckIncreasing <- function(x, name) {
  if (is.null(x) || length(x) < 2 || !is.numeric(x) || anyNA(x))
    return(NULL)
  if (any(diff(x) <= 0))
    return(paste0('`', name, '` must be strictly increasing.'))
  NULL
}

# bounds on model parameters, by parameter name: lower, upper, lower open, upper open
.ParBounds <- list(
  Linf       = c(0, Inf, 1, 0),
  K          = c(0, Inf, 1, 0),
  L0         = c(0, Inf, 0, 0),
  g          = c(0, Inf, 1, 0),
  d          = c(0, Inf, 1, 0),
  alpha      = c(0, Inf, 1, 0),
  beta       = c(0, Inf, 1, 0),
  Alpha      = c(0, Inf, 1, 0),
  Beta       = c(0, Inf, 1, 0),
  M          = c(0, Inf, 0, 0),
  Mref       = c(0, Inf, 1, 0),
  Lref       = c(0, Inf, 1, 0),
  Wref       = c(0, Inf, 1, 0),
  A50        = c(0, Inf, 0, 0),
  A50_95     = c(0, Inf, 1, 0),
  L50        = c(0, Inf, 1, 0),
  L50_95     = c(0, Inf, 1, 0),
  W50        = c(0, Inf, 1, 0),
  W50_95     = c(0, Inf, 1, 0),
  MaxFec     = c(0, Inf, 1, 0),
  SA50       = c(0, Inf, 0, 0),
  SA50_95    = c(0, Inf, 1, 0),
  SL50       = c(0, Inf, 1, 0),
  SL50_95    = c(0, Inf, 1, 0),
  SW50       = c(0, Inf, 1, 0),
  SW50_95    = c(0, Inf, 1, 0),
  SA         = c(0, Inf, 0, 0),
  SL         = c(0, Inf, 0, 0),
  L5         = c(0, Inf, 0, 0),
  LFS        = c(0, Inf, 0, 0),
  Vmaxlen    = c(0, 1, 0, 0),
  W5         = c(0, Inf, 0, 0),
  WFS        = c(0, Inf, 0, 0),
  Vmaxweight = c(0, 1, 0, 0),
  RA50       = c(0, Inf, 0, 0),
  RA50_95    = c(0, Inf, 1, 0),
  RL50       = c(0, Inf, 1, 0),
  RL50_95    = c(0, Inf, 1, 0),
  RW50       = c(0, Inf, 1, 0),
  RW50_95    = c(0, Inf, 1, 0),
  RA         = c(0, Inf, 0, 0),
  RL         = c(0, Inf, 0, 0),
  MaxRet     = c(0, 1, 0, 0),
  LR5        = c(0, Inf, 0, 0),
  LFR        = c(0, Inf, 0, 0),
  Rmaxlen    = c(0, 1, 0, 0),
  WR5        = c(0, Inf, 0, 0),
  WFR        = c(0, Inf, 0, 0),
  Rmaxweight = c(0, 1, 0, 0),
  h          = c(0.2, 1, 0, 1),
  hR         = c(0.2, Inf, 1, 0),
  Shinge     = c(0, 1, 1, 0)
)

.ParsProblems <- function(object) {
  Pars <- object@Pars
  if (is.null(Pars) || !length(Pars))
    return(NULL)
  if (!is.list(Pars))
    return('`Pars` must be a named list.')
  Names <- names(Pars)
  if (is.null(Names) || any(!nzchar(Names)))
    return('`Pars` must be a named list; every element needs a name.')
  if (anyDuplicated(Names))
    return(paste0('`Pars` has duplicated names: ', .FormatVals(Names[duplicated(Names)]), '.'))
  if (all(vapply(Pars, \(p) !length(p) || all(is.na(p)), logical(1))))
    return(NULL)

  Custom  <- is.function(object@Model)
  SDNames <- Names[tolower(.SubstrRight(Names, 2)) == 'sd']
  Orphans <- SDNames[!substr(SDNames, 1, nchar(SDNames) - 2) %in% Names]
  Out     <- if (length(Orphans))
    paste0('`Pars$', Orphans, '` is a random-walk SD but `Pars` has no parameter `',
           substr(Orphans, 1, nchar(Orphans) - 2), '`.')
  for (nm in Names) {
    p   <- Pars[[nm]]
    lab <- paste0('Pars$', nm)
    if (Custom && !is.numeric(p))
      next
    if (!length(p)) {
      Out <- c(Out, paste0('`', lab, '` is empty.'))
      next
    }
    IsSD <- tolower(.SubstrRight(nm, 2)) == 'sd' || .IsCVPar(nm)
    b    <- if (Custom) NULL else .ParBounds[[nm]]
    Out  <- c(Out, if (IsSD) {
      .CheckNum(p, lab, 0)
    } else if (!is.null(b)) {
      .CheckNum(p, lab, b[1], b[2], b[3] == 1, b[4] == 1)
    } else {
      .CheckNum(p, lab)
    })
  }
  Out
}

# a character `Model` must name one of the models of its class
.ModelProblems <- function(object) {
  Model <- object@Model
  if (is.null(Model) || is.function(Model) || !length(Model))
    return(NULL)
  if (!is.character(Model) || length(Model) != 1)
    return('`Model` must be a single model name or a function.')
  if (is.na(Model) || !nzchar(Model))
    return(NULL)
  cl    <- class(object)[1]
  Valid <- .ValidModelNames(object)
  if (is.null(Valid) || Model %in% Valid)
    return(NULL)
  .CheckChoice(Model, 'Model', Valid, help = paste0('`', .ModelHelpTopic[[cl]] %||% 'Models', '()`'))
}

.ValidModelNames <- function(object) {
  cl    <- class(object)[1]
  Slots <- c(methods::slotNames('stock'), methods::slotNames('fleet'))
  Ind   <- match(cl, tolower(Slots))
  if (is.na(Ind))
    return(NULL)
  Fun <- get0(paste0(Slots[Ind], 'Models'), envir = asNamespace('MSEtool'), mode = 'function')
  if (is.null(Fun))
    return(NULL)
  Fun(FALSE, FALSE)
}

.SizeClassProblems <- function(object) {
  c(.CheckNum(object@Classes, 'Classes', 0),
    .CheckIncreasing(object@Classes, 'Classes'))
}

.InputProblems_ages <- function(object) {
  c(.CheckNum(object@MaxAge, 'MaxAge', 0, len = 1),
    .CheckNum(object@MinAge, 'MinAge', 0, len = 1),
    .CheckFlag(object@PlusGroup, 'PlusGroup'),
    .CheckChoice(object@Units, 'Units', ValidUnits()))
}

.InputProblems_length <- function(object) {
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckChoice(object@Units, 'Units', ValidUnits('length'), help = '`ValidUnits("length")`'),
    .CheckNum(object@MeanAtAge, 'MeanAtAge', 0),
    .CheckNum(object@CVatAge, 'CVatAge', 0),
    .CheckChoice(object@Dist, 'Dist', c('normal', 'lognormal'), required = TRUE),
    .CheckNum(object@TruncSD, 'TruncSD', 0, lowerOpen = TRUE),
    .CheckNum(object@Timing, 'Timing', 0, 1),
    .SizeClassProblems(object))
}

.InputProblems_weight <- function(object) {
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckChoice(object@Units, 'Units', ValidUnits('weight'), help = '`ValidUnits("weight")`'),
    .CheckNum(object@MeanAtAge, 'MeanAtAge', 0),
    .CheckNum(object@MeanAtLength, 'MeanAtLength', 0),
    .CheckNum(object@CVatAge, 'CVatAge', 0),
    .CheckChoice(object@Dist, 'Dist', c('normal', 'lognormal')),
    .CheckNum(object@TruncSD, 'TruncSD', 0, lowerOpen = TRUE),
    .CheckNum(object@Timing, 'Timing', 0, 1),
    .SizeClassProblems(object))
}

.InputProblems_naturalmortality <- function(object) {
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckNum(object@MeanAtAge, 'MeanAtAge', 0),
    .CheckNum(object@MeanAtLength, 'MeanAtLength', 0),
    .CheckNum(object@MeanAtWeight, 'MeanAtWeight', 0),
    .CheckChoice(object@Units, 'Units', ValidUnits()),
    .SizeClassProblems(object))
}

.InputProblems_maturity <- function(object) {
  Semel <- object@Semelparous
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckNum(object@MeanAtAge, 'MeanAtAge', 0, 1),
    .CheckNum(object@MeanAtLength, 'MeanAtLength', 0, 1),
    .CheckNum(object@MeanAtWeight, 'MeanAtWeight', 0, 1),
    .SizeClassProblems(object),
    if (is.logical(Semel) && length(Semel) <= 1) .CheckFlag(Semel, 'Semelparous')
    else .CheckNum(Semel, 'Semelparous', 0, 1))
}

.InputProblems_fecundity <- function(object) {
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckNum(object@MeanAtAge, 'MeanAtAge', 0),
    .CheckNum(object@MeanAtLength, 'MeanAtLength', 0),
    .CheckNum(object@MeanAtWeight, 'MeanAtWeight', 0),
    .SizeClassProblems(object))
}

.InputProblems_srr <- function(object) {
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckNum(object@R0, 'R0', 0),
    .CheckNum(object@SD, 'SD', 0),
    .CheckNum(object@AC, 'AC', -1, 1, TRUE, TRUE),
    .CheckNum(object@TruncSD, 'TruncSD', 0, lowerOpen = TRUE),
    .CheckNum(object@SpawnTimeFrac, 'SpawnTimeFrac', 0, 1),
    .CheckNum(object@SpawnLag, 'SpawnLag', 0, whole = TRUE),
    .CheckNum(object@Units, 'Units', 0, lowerOpen = TRUE),
    .CheckFlag(object@ShareSPFrom, 'ShareSPFrom'),
    .RelRecFunProblems(object@RelRecFun),
    .CheckNum(object@RecDevInit, 'RecDevInit', 0, lowerOpen = TRUE, na = 'any'),
    .CheckNum(object@RecDevHist, 'RecDevHist', 0, lowerOpen = TRUE, na = 'any'),
    .CheckNum(object@RecDevProj, 'RecDevProj', 0, lowerOpen = TRUE, na = 'any'))
}

.RelRecFunProblems <- function(RelRecFun) {
  if (!is.character(RelRecFun) || !length(RelRecFun))
    return(NULL)
  if (length(RelRecFun) == 1 && !is.na(RelRecFun) &&
      !is.null(get0(RelRecFun, envir = asNamespace('MSEtool'), mode = 'function')))
    return(NULL)
  paste0('`RelRecFun` "', paste(RelRecFun, collapse = ' '), '" is not a function. ',
         'Leave it `NULL` to use the relative-recruitment function of `Model`, or supply a function.')
}

.InputProblems_fleet <- function(object) {
  c(.CheckNum(object@WeightFleetSelected, 'WeightFleetSelected', 0, na = 'all'),
    .CheckNum(object@WeightFleetRetained, 'WeightFleetRetained', 0, na = 'all'))
}

.InputProblems_spatial <- function(object) {
  RelSize <- object@RelativeSize
  c(.CheckNum(object@UnfishedDist, 'UnfishedDist', 0, 1),
    .CheckNum(object@ProbStaying, 'ProbStaying', 0, 1),
    .CheckNum(object@Movement, 'Movement', 0, 1),
    .CheckNum(object@FracOther, 'FracOther', 0, na = 'any'),
    if (is.character(RelSize)) .CheckChoice(RelSize, 'RelativeSize', 'EqualDensity')
    else .CheckNum(RelSize, 'RelativeSize', 0, lowerOpen = TRUE),
    .CheckNum(object@CVDist, 'CVDist', 0, lowerOpen = TRUE, len = 1),
    .CheckNum(object@CVStay, 'CVStay', 0, lowerOpen = TRUE, len = 1))
}

.InputProblems_depletion <- function(object) {
  c(.CheckNum(object@Initial, 'Initial', 0, lowerOpen = TRUE, na = 'all'),
    .CheckNum(object@Final, 'Final', 0, lowerOpen = TRUE, na = 'all'),
    .CheckChoice(object@Reference, 'Reference', .ValidDepletionReference, required = TRUE))
}

.InputProblems_effort <- function(object) {
  c(if (!is.data.frame(object@Effort)) .CheckNum(object@Effort, 'Effort', 0, na = 'any'),
    .CheckNum(object@Distribution, 'Distribution', 0, 1, na = 'any'),
    .CheckNum(object@Targeting, 'Targeting'),
    .CheckNum(object@ReleaseValue, 'ReleaseValue'),
    .CheckNum(object@Maximum, 'Maximum', 0, lowerOpen = TRUE, na = 'any'),
    .CheckChoice(object@Mode, 'Mode', c('Density', 'Biomass'), required = TRUE),
    .CheckNum(object@TripsScalar, 'TripsScalar', 0),
    .CheckNum(object@AnglerPerTrip, 'AnglerPerTrip', 0, lowerOpen = TRUE),
    .CheckNum(object@Theta, 'Theta', 0, lowerOpen = TRUE),
    .CheckNum(object@StockTargetingLambda, 'StockTargetingLambda', 0))
}

.InputProblems_catchability <- function(object) {
  c(.CheckNum(object@Efficiency, 'Efficiency', 0, na = 'all'),
    .CheckNum(object@qCV, 'qCV', 0),
    .CheckNum(object@qInc, 'qInc', -100, lowerOpen = TRUE))
}

.InputProblems_selectivity <- function(object) {
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckNum(object@MeanAtAge, 'MeanAtAge', 0),
    .CheckNum(object@MeanAtLength, 'MeanAtLength', 0),
    .CheckNum(object@MeanAtWeight, 'MeanAtWeight', 0),
    .SizeClassProblems(object))
}

.InputProblems_retention <- function(object) {
  c(.ParsProblems(object),
    .ModelProblems(object),
    .CheckNum(object@MeanAtAge, 'MeanAtAge', 0, 1),
    .CheckNum(object@MeanAtLength, 'MeanAtLength', 0, 1),
    .CheckNum(object@MeanAtWeight, 'MeanAtWeight', 0, 1),
    .SizeClassProblems(object))
}

.InputProblems_discardmortality <- function(object) {
  c(.CheckNum(object@MeanAtAge, 'MeanAtAge', 0, 1),
    .CheckNum(object@MeanAtLength, 'MeanAtLength', 0, 1),
    .SizeClassProblems(object))
}

.InputProblems_impslot <- function(object) {
  c(.CheckNum(object@Mean, 'Mean', 0, na = 'all'),
    .CheckNum(object@SD, 'SD', 0, na = 'all'),
    .CheckNum(object@Compliance, 'Compliance', 0, 1, na = 'all'),
    .CheckNum(object@Error, 'Error', 0, na = 'all'))
}

.InputProblems_imp <- function(object) {
  unlist(lapply(c('TAC', 'Effort', 'Size'), \(sl) {
    Slot <- methods::slot(object, sl)
    if (!methods::is(Slot, 'impslot'))
      return(NULL)
    P <- .InputProblems(Slot)
    if (length(P)) paste0(sl, ': ', P)
  }))
}

.ObsCommonProblems <- function(object) {
  c(.CheckNum(object@CV, 'CV', 0, na = 'all'),
    .CheckNum(object@Bias, 'Bias', 0, lowerOpen = TRUE, na = 'all'),
    .CheckNum(object@Error, 'Error', 0, lowerOpen = TRUE, na = 'any'),
    .CheckNum(object@Ref, 'Ref', 0, na = 'all'),
    .CheckNum(object@Years, 'Years'))
}

.InputProblems_effortobs <- function(object) .ObsCommonProblems(object)

.InputProblems_catchobs <- function(object) .ObsCommonProblems(object)

.InputProblems_indicesobs <- function(object) {
  Sel <- object@Selectivity
  c(.CheckNum(object@CV, 'CV', 0, na = 'all'),
    .CheckNum(object@Error, 'Error', 0, lowerOpen = TRUE, na = 'any'),
    .CheckNum(object@Beta, 'Beta', 0, lowerOpen = TRUE, na = 'all'),
    .CheckNum(object@AC, 'AC', -1, 1, TRUE, TRUE, na = 'all'),
    .CheckNum(object@Ref, 'Ref', 0, na = 'all'),
    .CheckNum(object@Efficiency, 'Efficiency', 0, lowerOpen = TRUE, na = 'all'),
    .CheckNum(object@TruncSD, 'TruncSD', 0, lowerOpen = TRUE, len = 1),
    .CheckNum(object@Areas, 'Areas', 1, whole = TRUE),
    .CheckNum(object@Years, 'Years'),
    .CheckChoice(object@Units, 'Units', c('Biomass', 'Number', 'Recruitment')),
    if (is.character(Sel)) .CheckChoice(Sel, 'Selectivity', c('Biomass', 'SBiomass')))
}

.InputProblems_compobs <- function(object) {
  c(.CheckNum(object@SampleSize, 'SampleSize', 0, lowerOpen = TRUE, na = 'all'),
    .CheckNum(object@ESS, 'ESS', 0, lowerOpen = TRUE, na = 'all'),
    .CheckNum(object@Theta, 'Theta', 0, 1, lowerOpen = TRUE, na = 'all'),
    .CheckNum(object@Shift, 'Shift', na = 'all'),
    .CheckNum(object@Years, 'Years'))
}

.CheckStockArgs <- function(nYear, pYear, CurrentYear, nSim, Seasons, label = NULL) {
  Problems <- c(.CheckNum(nYear, 'nYear', 1, len = 1, whole = TRUE),
                .CheckNum(pYear, 'pYear', 0, len = 1, whole = TRUE),
                .CheckNum(CurrentYear, 'CurrentYear', 1, len = 1, whole = TRUE),
                .CheckNum(nSim, 'nSim', 1, len = 1, whole = TRUE),
                .CheckNum(Seasons, 'Seasons', 1, len = 1, whole = TRUE))
  if (length(Problems))
    .AbortInvalidInputs(Problems, 'stock', label)
  invisible(TRUE)
}

.InputProblems_om <- function(object) {
  c(.CheckNum(object@nSim, 'nSim', 1, len = 1, whole = TRUE),
    .CheckNum(object@nYear, 'nYear', 1, len = 1, whole = TRUE),
    .CheckNum(object@pYear, 'pYear', 0, len = 1, whole = TRUE),
    .CheckNum(object@CurrentYear, 'CurrentYear', 1, len = 1, whole = TRUE),
    .CheckNum(object@Seasons, 'Seasons', 1, len = 1, whole = TRUE),
    .CheckNum(object@maxF, 'maxF', 0, lowerOpen = TRUE, len = 1),
    .CheckNum(object@nReps, 'nReps', 1, len = 1, whole = TRUE),
    .CheckNum(object@pStar, 'pStar', 0, 1, TRUE, TRUE, len = 1),
    .CheckNum(object@Interval, 'Interval', 1, whole = TRUE),
    .CheckNum(object@Seed, 'Seed'))
}

# names an unnamed array by position: a lower-dimensional input fills `Required` first, then Sim, Year, Area in order; a missing Sim is 1 and a missing Year is the first year
.NameByPosition <- function(x, Dims, Required, Labels, Sizes, name, cl = 'object') {
  if (is.null(x) || !length(x) || !is.numeric(x) || (is.null(dim(x)) && length(x) == 1))
    return(x)
  if (!is.null(names(dimnames(x))))
    return(.OrderNamedDims(x, Dims, Required, Labels, name, cl))

  dd       <- dim(x) %||% length(x)
  Optional <- intersect(c('Sim', 'Age', 'Year', 'Area'), setdiff(Dims, Required))
  k        <- length(dd)
  if (k < length(Required) || k > length(Dims))
    .AbortInvalidInputs(paste0('`', name, '` has ', k, ' dimension(s); an unnamed array is read by position as ',
                               paste(Dims, collapse = ' x '), ' and must include ',
                               paste(Required, collapse = ' x '), '.'), cl)
  Use <- Dims[Dims %in% c(Required, Optional[seq_len(k - length(Required))])]
  .CheckUnnamedDims(dd, name, cl, Sizes[Use])
  DN  <- stats::setNames(lapply(seq_along(Use), \(i) {
    Lab <- Labels[[Use[i]]]
    if (is.null(Lab)) seq_len(dd[i]) else Lab[seq_len(dd[i])]
  }), Use)
  Out <- array(as.numeric(x), dim = dd, dimnames = DN)
  .AddMissingDims(Out, Dims, Labels)
}

# number of historical time steps attached to `Years` by PopulateStock()/PopulateFleet()
.YearSizes <- function(Years, nHist = attr(Years, 'nHist')) unique(c(nHist, length(Years)))

.WithNHist <- function(Years, nHist) {
  attr(Years, 'nHist') <- nHist
  Years
}

.NameAtAge <- function(x, Ages, Years, nArea = NULL, name = 'MeanAtAge', cl = 'object') {
  Labels <- list(
    Age  = Ages@Classes,
    Year = as.numeric(Years)
  )
  Sizes  <- list(
    Sim  = NA,
    Age  = length(Ages@Classes),
    Year = .YearSizes(Years),
    Area = nArea
  )
  .NameByPosition(x, c('Sim', 'Age', 'Year', if (!is.null(nArea)) 'Area'), 'Age',
                  Labels, Sizes, name, cl)
}

.NameSimYear <- function(x, Years, name, cl = 'object', nHist = attr(Years, 'nHist')) {
  Sizes <- list(
    Sim  = NA,
    Year = .YearSizes(Years, nHist)
  )
  .NameByPosition(x, c('Sim', 'Year'), 'Year', list(Year = as.numeric(Years)), Sizes, name, cl)
}

.NameSimYearArea <- function(x, Years, nArea, name, cl = 'object', nHist = attr(Years, 'nHist')) {
  Sizes <- list(
    Sim  = NA,
    Year = .YearSizes(Years, nHist),
    Area = nArea
  )
  .NameByPosition(x, c('Sim', 'Year', 'Area'), 'Area', list(Year = as.numeric(Years)), Sizes, name, cl)
}

`%|NA|%` <- function(x, y) if (is.na(x)) y else x

# names an unnamed at-size array by position (Sim, Class, Year, then Area); classes are the object's own when they fit, else those of `Ref` (the Length or Weight object)
.NameAtSize <- function(object, slotName, Ref, Years, nArea = NULL) {
  x <- methods::slot(object, slotName)
  if (is.null(x) || !length(x) || !is.numeric(x))
    return(object)
  Dims <- c('Sim', 'Class', 'Year', if (!is.null(nArea)) 'Area')
  if (!is.null(names(dimnames(x)))) {
    x <- .OrderNamedDims(x, Dims, 'Class', list(Year = as.numeric(Years)), slotName, class(object)[1])
    methods::slot(object, slotName) <- x
    if (methods::.hasSlot(object, 'Classes') && !length(object@Classes) && !methods::is(object, 'weight'))
      object@Classes <- as.numeric(dimnames(x)$Class)
    return(object)
  }

  Own        <- if (methods::.hasSlot(object, 'Classes')) object@Classes
  RefClasses <- if (isS4(Ref) && methods::.hasSlot(Ref, 'Classes')) Ref@Classes
  Cls        <- if (length(Own) && !(methods::is(object, 'weight') && slotName == 'MeanAtLength')) Own else RefClasses
  if (!length(Cls))
    return(object)
  Labels <- list(
    Class = Cls,
    Year  = as.numeric(Years)
  )
  Sizes  <- list(
    Sim   = NA,
    Class = length(Cls),
    Year  = .YearSizes(Years),
    Area  = nArea
  )
  methods::slot(object, slotName) <- .NameByPosition(x, Dims, 'Class', Labels, Sizes, slotName, class(object)[1])
  if (!length(Own) && !methods::is(object, 'weight'))
    object@Classes <- Cls
  object
}

# a named array is put in the standard dimension order, with `Length`/`Weight` read as `Class` and a missing Sim or Year added
.OrderNamedDims <- function(x, Dims, Required, Labels, name, cl = 'object') {
  N <- names(dimnames(x))
  if ('Class' %in% Dims)
    N[N %in% c('Length', 'Weight')] <- 'Class'
  names(dimnames(x)) <- N
  Bad     <- setdiff(N, Dims)
  Missing <- setdiff(Required, N)
  if (length(Bad) || length(Missing) || anyDuplicated(N) || any(!nzchar(N)))
    .AbortInvalidInputs(paste0('`', name, '` has dimensions ', paste(N, collapse = ' x '),
                               '; named dimensions must be from ', paste(Dims, collapse = ' x '),
                               if (length(Required)) paste0(' and include ', paste(Required, collapse = ' x ')),
                               '.'), cl)
  .AddMissingDims(x, Dims, Labels)
}

# adds any missing Sim, Age, Year, or Area dimension with length 1 and puts the dimensions in `Dims` order
.AddMissingDims <- function(x, Dims, Labels) {
  for (d in setdiff(intersect(c('Sim', 'Age', 'Year', 'Area'), Dims), names(dimnames(x))))
    x <- AddDimension(x, d, val = if (d %in% c('Age', 'Year')) Labels[[d]][1] else 1)
  Order <- intersect(Dims, names(dimnames(x)))
  if (identical(names(dimnames(x)), Order)) x else aperm(x, Order)
}

# unnamed spatial arrays are read by position; vectors keep their scalar, bounds, and per-area meanings
.NameSpatial <- function(Spatial, Ages, Years) {
  Labels <- list(
    Age  = Ages@Classes,
    Year = as.numeric(Years)
  )
  Sizes  <- list(
    Sim      = NA,
    Area     = NA,
    FromArea = NA,
    ToArea   = NA,
    Age      = length(Ages@Classes),
    Year     = .YearSizes(Years)
  )
  ByArea <- c('Sim', 'Area', 'Age', 'Year')
  ByMove <- c('Sim', 'FromArea', 'ToArea', 'Age', 'Year')
  Specs  <- list(
    UnfishedDist = list(ByArea, 'Area'),
    ProbStaying  = list(ByArea, 'Area'),
    RelativeSize = list(c('Sim', 'Area'), 'Area'),
    Movement     = list(ByMove, c('FromArea', 'ToArea')),
    FracOther    = list(ByMove, c('FromArea', 'ToArea'))
  )
  for (sl in names(Specs)) {
    x <- methods::slot(Spatial, sl)
    if (!is.array(x) || !is.numeric(x))
      next
    Dims <- Specs[[sl]][[1]]
    methods::slot(Spatial, sl) <- .NameByPosition(x, Dims, Specs[[sl]][[2]], Labels, Sizes[Dims], sl, 'spatial')
  }
  Spatial
}

# a CVatAge vector is a scalar, uniform bounds, or one value per age class
.CheckCVatAgeLength <- function(CVatAge, Ages, cl) {
  if (is.null(CVatAge) || !is.null(dim(CVatAge)) || length(CVatAge) %in% c(1, 2, length(Ages@Classes)))
    return(invisible(TRUE))
  .AbortInvalidInputs(paste0('`CVatAge` has ', length(CVatAge), ' values; a vector must have 1 (constant), ',
                             '2 (uniform bounds), or ', length(Ages@Classes),
                             ' (one per age class) values. Use a named `Sim` x `Age` x `Year` array for other forms.'), cl)
}
