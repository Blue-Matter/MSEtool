OnExit <- function() {
  do.call(on.exit, list(cli::stop_app(), add = TRUE),
          envir = parent.frame())
}

SetSeed <- function(seed = NULL) {
  if (is.null(seed)) {
    seed <- 101
  }
  set.seed(seed)
}

not <- function(val) !val

ReplaceTiny <- function(Array, value = 1, default = tiny / 2) {
  Array[Array == default] <- value
  Array
}

range01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

aperm <- function(a, perm, ...) {
  if (is.null(a) || length(a) < 1) {
    return(a)
  }
  base::aperm(a, perm, ...)
}

# Transformations -----

logit <- function(p) {
  log(p / (1 - p))
}

ilogit <- function(x) {
  1 / (1 + exp(-x))
}

# Distributions -----

# truncated normal distribution
ptnorm <- function(q, mean, sd, truncsd) {
  a <- (-truncsd * sd) + mean
  b <- (truncsd * sd) + mean
  out <- vector("numeric", length(a))
  for (i in seq_along(a)) {
    if (q < a[i]) {
      out[i] <- 0
    } else if (q > b[i]) {
      out[i] <- 1
    } else {
      p1 <- stats::pnorm(q, mean[i], sd[i], TRUE, FALSE)
      p2 <- stats::pnorm(a[i], mean[i], sd[i], TRUE, FALSE)
      p3 <- stats::pnorm(b[i], mean[i], sd[i], TRUE, FALSE)
      out[i] <- (p1 - p2) / (p3 - p2)
    }
  }
  out
}

rtnorm <- function(n, mu, sigma, lower, upper) {
  qnorm(
    runif(
      n,
      pnorm(lower, mu, sigma),
      pnorm(upper, mu, sigma)
    ),
    mu,
    sigma
  )
}

# ---- Text ----
firstup <- function(x, n = 1) {
  substr(x, 1, n) <- toupper(substr(x, 1, n))
  x
}

# ---- Messages ----

# default: info, progress, warnings
# FALSE: no messages or warnings
# minimal:

SetMessages <- function(messages = "default") {
  msg <- list()
  if (isFALSE(messages)) {
    return(msg)
  }

  msg$info <- TRUE
  msg$alert <- TRUE
  msg$progress <- TRUE
  msg$warning <- TRUE

  msg
}

StartMessages <- function(OM, messages = "default") {
  msg <- SetMessages(messages)

  # Allocation

  # if (!length(OM@Allocation)) {
  #   OM@Allocation <- OM@CatchFrac
  #   if (nFleet(OM)>1) {
  #     if (isTRUE(msg$alert))
  #       cli::cli(c(
  #         cli::cli_alert_info('`Allocation(OM)` not specified'),
  #         cli::cli_alert('Setting `Allocation` equal to `CatchFrac` (`Allocate(OM) <- CatchFrac(OM)`)')
  #       ))
  #   }
  # }

  if (!length(OM@EFactor)) {
    OM@EFactor <- lapply(1:nStock(OM), function(x) {
      matrix(1, nSim(OM), nFleet(OM))
    })
    if (nFleet(OM) > 1) {
      if (isTRUE(msg$alert)) {
        cli::cli(c(
          cli::cli_alert_info("`EFactor(OM)` not specified"),
          cli::cli_alert("Setting `EFactor(OM)` to current effort for all fleets")
        ))
      }
    }
  }


  # if (nStock(OM)>1 && !length(OM@Relations) && !length(OM@Herm)) {
  #   if (isTRUE(msg$alert)) {
  #     cli::cli_alert_info("You have specified more than one stock but no MICE relationships (`Relations(OM)`) or sex-specific relationships (`SexPars(OM)`) among these. \nAs they are independent, consider doing MSE for one stock at a time for computational efficiency\n")
  #   }
  # }
  OM
}

getModelClass <- function(Model = NULL) {
  if (is.null(Model)) {
    return(NULL)
  }
  if (inherits(Model, "function")) {
    return("function")
  }
  class(get(Model))
}

CalcTSUnits <- function(Seasons) {
  if (is.null(Seasons)) {
    return("year")
  }

  out <- switch(as.character(Seasons),
    "1" = "year",
    "2" = "half-year",
    "4" = "quarter",
    "12" = "month",
    "52" = "week",
    "365" = "day"
  )

  if (is.null(out)) {
    cli::cli_abort(c(
      "x" = "`Seasons`: {.val {Seasons}} is invalid ",
      "i" = "Must be one of {.val {c(1,2,4,12,52,365)}}"
    ))
  }
  out
}

CalcSeasons <- function(Units) {
  Units <- tolower(Units)
  switch(Units,
    "year" = 1,
    "half-year" = 2,
    "quarter" = 4,
    "month" = 12,
    "week" = 52
  )
}

CalcYears <- function(nYear, pYear, CurrentYear, Seasons = 1, Period = NULL) {
  TimeUnits <- CalcTSUnits(Seasons)

  if (CurrentYear < 1900 && TimeUnits == "year") {
    # not in year units
    hist <- seq(CurrentYear, by = -1, length.out = nYear) |> rev()
    proj <- seq(CurrentYear + 1, by = 1, length.out = pYear)

    if (is.null(Period)) {
      return(c(hist, proj))
    }

    if (grepl("H", Period)) {
      return(hist)
    }

    if (grepl("P", Period)) {
      return(proj)
    }
  }

  if (TimeUnits == "year") {
    FirstHistYear <- CurrentYear - nYear + 1
    LastHistYear <- CurrentYear

    FirstProjYear <- CurrentYear + 1
    LastProjYear <- CurrentYear + pYear

    hist <- FirstHistYear:LastHistYear
    proj <- FirstProjYear:LastProjYear

    if (is.null(Period)) {
      return(c(hist, proj))
    }

    if (grepl("H", Period)) {
      return(hist)
    }

    if (grepl("P", Period)) {
      return(proj)
    }
  }

  FirstHistYear <- lubridate::ymd(paste0(CurrentYear - nYear + 1, "-01-01"))
  LastHistYear <- lubridate::ymd(paste0(CurrentYear, "-12-31"))

  FirstProjYear <- lubridate::ymd(paste0(CurrentYear + 1, "-01-01"))
  LastProjYear <- lubridate::ymd(paste0(CurrentYear + pYear, "-12-31"))
  validTimeUnits <- c("year", "half-year", "quarter", "month", "week", "day")

  if (TimeUnits == "half-year") {
    hist <- seq(FirstHistYear, LastHistYear, by = "6 months") |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by = "6 months") |> lubridate::decimal_date()
  } else if (TimeUnits == "quarter") {
    hist <- seq(FirstHistYear, LastHistYear, by = "3 months") |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by = "3 months") |> lubridate::decimal_date()
  } else if (TimeUnits == "month") {
    hist <- seq(FirstHistYear, LastHistYear, by = "1 month") |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by = "1 month") |> lubridate::decimal_date()
  } else if (TimeUnits == "week") {
    hist <- seq(FirstHistYear, LastHistYear, by = "1 week") |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by = "1 week") |> lubridate::decimal_date()
  } else if (TimeUnits == "day") {
    hist <- seq(FirstHistYear, LastHistYear, by = "1 day") |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by = "1 day") |> lubridate::decimal_date()
  } else {
    cli::cli_abort("`TimeUnits` must be one of: {.val {validTimeUnits}}")
  }

  hist <- hist |> round(4)
  proj <- proj |> round(4)

  if (is.null(Period)) {
    return(c(hist, proj))
  }

  if (grepl("H", Period)) {
    return(hist)
  }

  if (grepl("P", Period)) {
    return(proj)
  }
}


ParsEmpty <- function(Pars) {
  !ParsNotEmpty(Pars)
}

ParsNotEmpty <- function(Pars) {
  if (length(Pars) == 0) {
    return(FALSE)
  }
  !prod(unlist(lapply(Pars, is.na)))
}

GetIndex <- function(i, max_i) {
  if (i >= max_i) {
    return(rep(1:max_i, i)[i])
  }
  i
}

IdenticalS4 <- function(object1, object2) {
  digest::digest(object1, algo = "spookyhash") == digest::digest(object2, algo = "spookyhash")
}

SetDigest <- function(object, argList = list()) {
  # object@Created <- NULL
  # object@Modified <- NULL

  if (is.list(argList)) {
    for (i in seq_along(argList)) {
      if (isS4(argList[[i]])) {
        # argList[[i]]@Created <- NULL
        # argList[[i]]@Modified <- NULL
      }
    }
  }

  attributes(object)$digest <- NULL
  attributes(object)$digest <- digest::digest(list(argList, object), algo = "spookyhash")
  object
}


MakeNamedList <- function(names, values = NULL) {
  l <- vector("list", length(names))
  names(l) <- names
  if (!is.null(values)) {
    for (i in 1:length(l)) {
      l[[i]] <- values
    }
  }
  l
}

CheckDigest <- function(object, argList = list()) {
  if (is.null(attributes(object)$digest)) {
    return(FALSE)
  }
  SetDigest <- SetDigest(object, argList)

  if (attributes(SetDigest)$digest == attributes(object)$digest) {
    return(TRUE)
  }
  FALSE
}

isNewObject <- function(object) {
  if (!isS4(object)) 
    return(FALSE)
  
  cl <- class(object)
  if (inherits(object, 'naturalmortality')) {
    newobj <- NaturalMortality()
  } else   if (inherits(object, 'srr')) {
    newobj <- SRR()
  } else   if (inherits(object, 'om')) {
    newobj <- OM()
  } else   if (inherits(object, 'discardmortality')) {
      newobj <- DiscardMortality()
  } else {
    chk <- try(get(firstup(cl)), silent=TRUE)
    if (inherits(chk, 'try-error')) {
      return(FALSE)
    }
    newobj <- get(firstup(cl))()
  }

  identical(object, newobj)
}

EmptyObject <- function(object) {
  if (isS4(object)) {
    if (isNewObject(object)) {
      return(TRUE)
    }

    sltnms <- slotNames(object)
    empty <- rep(TRUE, length(sltnms))
    for (i in seq_along(sltnms)) {
      sl <- sltnms[i]
      val <- slot(object, sl)
      if (inherits(val, "function")) {
        next()
      }
      if (isS4(val)) {
        empty[i] <- Recall(val)
      } else {
        empty[i] <- is.null(val) || length(val) == 0 || all(is.na(val))
      }
    }
    return(prod(empty))
  }
  length(object) < 1 | all(is.na(object))
}

# 
# AddDimNames <- function(array, names = c("Sim", "Age", "Year"),
#                         Years = NULL, Ages = NULL, Fleets = NULL,
#                         values = NULL) {
#   if (inherits(array, "list")) {
#     array <- unlist(array)
#   }
#   if (is.null(array)) {
#     return(array)
#   }
#   d <- dim(array)
#   l <- list()
#   for (i in seq_along(names)) {
#     if (names[i] == "Age") {
#       if (!is.null(Ages)) {
#         l[[i]] <- Ages
#       } else {
#         l[[i]] <- 0:(d[i] - 1)
#       }
#     } else if (names[i] == "Fleet" && !is.null(Fleets)) {
#       l[[i]] <- Fleets
#     } else if (names[i] == "Year" && !is.null(Years)) {
#       l[[i]] <- Years[1:d[i]]
#     } else {
#       if (is.null(values)) {
#         l[[i]] <- 1:d[i]
#       } else {
#         if (!is.null(values[[i]]) && !any(is.na(values[[i]]))) {
#           l[[i]] <- values[[i]]
#         } else {
#           l[[i]] <- 1:d[i]
#         }
#       }
#     }
#   }
# 
#   names(l) <- names
#   dimnames(array) <- l
#   array
# }

# AddMeanAtAgeAttributes <- function(object, Years=NULL, Ages=NULL) {
#
#   object@MeanAtAge <- Structure(value=object@MeanAtAge,
#                                 out=c('nsim', 'nage', 'nTS'))
#
#   if (is.null(dimnames(object@MeanAtAge)))
#     object@MeanAtAge <- object@MeanAtAge |> AddDimNames(Years=Years)
#
#   if ('Units' %in% slotNames(object))
#     attributes(object@MeanAtAge)$Units <- object@Units
#
#   # if (is.null(attributes(object@MeanAtAge)$Years))
#   #   attributes(object@MeanAtAge)$Years <- Years
#   #
#   if (methods::is(Ages, 'ages')) {
#     # attributes(object@MeanAtAge)$Ages <- Ages@Classes
#     attributes(object@MeanAtAge)$UnitsAge <- Ages@Units
#   }
#   object
# }


