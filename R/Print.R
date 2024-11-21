#' Generic show method
#'
#' @param object Object to print to console
#' @export
show <- function(object) methods::show(object)

## --- Ages ----

#' @rdname show
setMethod('show', 'ages', function(object) {
  cli::cli_par()
  cli::cli_h2("An {.help MSEtool::Ages} Object")

  cli::cli_h3('{.code MaxAge}')
  cli::cli_text("{.val {object@MaxAge}}")

  if (length(object@MaxAge)>0) {
    if (is.null(object@Classes)) {
      object@Classes <- 0:object@MaxAge
    }
  }
  cli::cli_h3('{.code Classes}')
  cli::cli_text("{.val {object@Classes}}")
  cli::cli_h3('{.code Units}')
  cli::cli_text("{.val {object@Units}}")
  cli::cli_h3('{.code PlusGroup}')
  cli::cli_text("{.val {object@PlusGroup}}")
  cli::cli_end()

  print(Check(object))

})

## ---- Length ----

setMethod('show', 'length', function(object) {

  cli::cli_par()
  cli::cli_h2("{.help MSEtool::Length} Object")
  cli::cli_h3('{.code Pars}')
  printPars(object@Pars)

  cli::cli_h3('{.code Model}')
  cli::cli_text('{.val { object@Model}}')

  cli::cli_h3('{.code Units}')
  cli::cli_text('{.val { object@Units}}')

  cli::cli_h3('{.code MeanAtAge}')
  printMeanatAge(object@MeanAtAge)

  cli::cli_h3('{.code CVatAge}')
  printMeanatAge(object@CVatAge)

  cli::cli_h3('{.code Dist}')
  cli::cli_text('{.val { object@Dist}}')

  cli::cli_h3('{.code TruncSD}')
  cli::cli_text('{.val { object@TruncSD}}')

  cli::cli_h3('{.code Timing}')
  cli::cli_text('{.val { object@Timing}}')

  cli::cli_h3('{.code ASK}')
  printASK(object@ASK)

  cli::cli_h3('{.code Classes}')
  if (!is.null(object@Classes)) {
    val <- cli::cli_vec(object@Classes, list("vec-trunc" = 10))
    cli::cli_text('nBins: {.val {length(object@Classes)}}')
    cli::cli_text('Classes: {.val { val}}')
  }


  cli::cli_end()
  print(Check(object))
})



## ---- Weight  ----
setMethod('show', 'weight', function(object) {

  cli::cli_par()
  cli::cli_h2("{.help MSEtool::Weight} Object")
  cli::cli_h3('{.code Pars}')
  printPars(object@Pars)

  cli::cli_h3('{.code Model}')
  cli::cli_text('{.val { object@Model}}')

  cli::cli_h3('{.code Units}')
  cli::cli_text('{.val { object@Units}}')

  cli::cli_h3('{.code MeanAtAge}')
  printMeanatAge(object@MeanAtAge)

  cli::cli_h3('{.code CVatAge}')
  printMeanatAge(object@CVatAge)

  cli::cli_h3('{.code Dist}')
  cli::cli_text('{.val { object@Dist}}')

  cli::cli_h3('{.code TruncSD}')
  cli::cli_text('{.val { object@TruncSD}}')

  cli::cli_h3('{.code Timing}')
  cli::cli_text('{.val { object@Timing}}')

  cli::cli_h3('{.code ASK}')
  printASK(object@ASK)

  cli::cli_h3('{.code Classes}')
  if (!is.null(object@Classes)) {
    val <- cli::cli_vec(object@Classes, list("vec-trunc" = 10))
    cli::cli_text('nBins: {.val {length(object@Classes)}}')
    cli::cli_text('{.val { val}}')
  }

  # cli::cli_h3('{.code Created}')
  # cli::cli_text('{.val { format(object@Created)}}')
  #
  # if (!is.null(object@Modified)) {
  #   cli::cli_h3('{.code Modified}')
  #   cli::cli_text('{.val { format(object@Modified)}}')
  # }

  cli::cli_end()
  print(Check(object))

})

## ---- NaturalMortality ----
setMethod('show', 'naturalmortality', function(object) {

  cli::cli_par()
  cli::cli_h2("{.help MSEtool::NaturalMortality} Object")
  cli::cli_h3('{.code Pars}')
  printPars(object@Pars)

  cli::cli_h3('{.code Model}')
  cli::cli_text('{.val { object@Model}}')

  cli::cli_h3('{.code Units}')
  cli::cli_text('{.val { object@Units}}')

  cli::cli_h3('{.code MeanAtAge}')
  printMeanatAge(object@MeanAtAge)

  cli::cli_h3('{.code MeanAtLength}')
  printMeanatAge(object@MeanAtLength, type="Length")

  cli::cli_h3('{.code Classes}')
  if (!is.null(object@Classes)) {
    val <- cli::cli_vec(object@Classes, list("vec-trunc" = 10))
    cli::cli_text('nBins: {.val {length(object@Classes)}}')
    cli::cli_text('{.val { val}}')
  }

  cli::cli_end()
  print(Check(object))
})



## ---- Maturity ----

#' @rdname show
setMethod('show', 'maturity', function(object) {

  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Maturity} Object")
  cli::cli_h3('{.code Pars}')
  printPars(object@Pars)

  cli::cli_h3('{.code Model}')
  cli::cli_text('{.val { object@Model}}')

  cli::cli_h3('{.code MeanAtAge}')
  printMeanatAge(object@MeanAtAge)

  cli::cli_h3('{.code MeanAtLength}')
  printMeanatAge(object@MeanAtLength, type='Length')

  cli::cli_h3('{.code Classes}')
  if (!is.null(object@Classes)) {
    val <- cli::cli_vec(object@Classes, list("vec-trunc" = 10))
    cli::cli_text('nBins: {.val {length(object@Classes)}}')
    cli::cli_text('{.val { val}}')
  }

  cli::cli_end()
  print(Check(object))

})


## ---- Fecundity ----

#' @rdname show
setMethod('show', 'fecundity', function(object) {

  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Fecundity} Object")
  cli::cli_h3('{.code Pars}')
  printPars(object@Pars)

  cli::cli_h3('{.code Model}')
  cli::cli_text('{.val { object@Model}}')

  cli::cli_h3('{.code Units}')
  cli::cli_text('{.val { object@Units}}')

  cli::cli_h3('{.code MeanAtAge}')
  printMeanatAge(object@MeanAtAge)

  cli::cli_h3('{.code MeanAtLength}')
  printMeanatAge(object@MeanAtLength, type='Length')

  cli::cli_h3('{.code Classes}')
  if (!is.null(object@Classes)) {
    val <- cli::cli_vec(object@Classes, list("vec-trunc" = 10))
    cli::cli_text('nBins: {.val {length(object@Classes)}}')
    cli::cli_text('{.val { val}}')
  }

  cli::cli_end()
  print(Check(object))

})

## ---- SRR ----

#' @rdname show
setMethod('show', 'srr', function(object) {

  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::SRR} Object")
  cli::cli_h3('{.code Pars}')
  printPars(object@Pars)

  cli::cli_h3('{.code Model}')
  cli::cli_text('{.val { object@Model}}')

  cli::cli_h3('{.code SD}')
  printPars(list(SD=object@SD))

  cli::cli_h3('{.code AC}')
  printPars(list(AC=object@AC))

  cli::cli_h3('{.code TruncSD}')
  cli::cli_text('{.val { object@TruncSD}}')

  cli::cli_h3('{.code RecDevInit}')
  printRecDevs(object@RecDevInit, type='init')

  cli::cli_h3('{.code RecDevHist}')
  printRecDevs(object@RecDevHist, type='hist')

  cli::cli_h3('{.code RecDevProj}')
  printRecDevs(object@RecDevProj, type='proj')

  cli::cli_h3('{.code SpawnTimeFrac}')
  cli::cli_text('{.val { object@SpawnTimeFrac}}')

  cli::cli_end()
  print(Check(object))

})

## ---- Spatial ----

#' @rdname show
setMethod('show', 'spatial', function(object) {

  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Spatial} Object")

  cli::cli_inform('...')

  cli::cli_end()
  print(Check(object))

})






## --- OM  ----
#' @describeIn show Print a [om-class()] object
setMethod("show", "om", function(object) {
  
  cli::cli_par()
  cli::cli_h2("An {.help MSEtool::OM} Object")
  
  cli::cli_h3('{.code Name}')
  cli::cli_text("{.val {object@Name}}")
  
  cli::cli_h3('Number of Stocks')
  cli::cli_text("{.val {nStock(object)}}")
  
  cli::cli_h3('Number of Fleets')
  cli::cli_text("{.val {nFleet(object)}}")
  
  cli::cli_end()
  
  print(Check(object))
})

## --- CheckList Object ----

#' @describeIn show Print a `CheckList` object
setMethod('show', 'CheckList', function(object) {

  cli::cli_h3('Checking')

  if (is.list(object@empty)) {
    print_list(object)
  } else {
    print_single(object)
  }
})




## --- Supporting Functions ----


printASK <- function(ASK) {
  if (is.null(ASK))
    return(NULL)
  dd <- dim(ASK)

  cli::cli_text('nsim: {.val { dd[1]}}')
  cli::cli_text('nAge: {.val { dd[2]}}')
  cli::cli_text('nBin: {.val { dd[3]}}')
  cli::cli_text('nTS: {.val { dd[4]}}')

  if (!is.null(attributes(ASK)$timesteps))
    cli::cli_text('Time Steps: {.val {attributes(ASK)$timesteps}}')

}

printRecDevs <- function(RecDevs,round=2, type='init') {

  dd <- dim(RecDevs)
  if (is.null(dd)) {
    dd <- c(1, length(RecDevs))
  }

  cli::cli_text('nsim: {.val { dd[1]}}')
  if (type=='init')
    cli::cli_text('MaxAge: {.val { dd[2]}}')
  if (type=='hist')
    cli::cli_text('nHistTS: {.val { dd[2]}}')
  if (type=='proj')
    cli::cli_text('nProjTS: {.val { dd[2]}}')

  if (dd[1]>1) {
    meanSim <- apply(RecDevs, 2, mean) |> round(round)
    val <- cli::cli_vec(meanSim, list("vec-trunc" = 10))
    cli::cli_text('Mean over simulations: {.val {val}}')
  } else {
    val <- cli::cli_vec(RecDevs, list("vec-trunc" = 10)) |> round(round)

    cli::cli_text('Deviations: {.val {val}}')
  }

}

printMeanatAge <- function(MeanAtAge, round=2, type='Age') {
  if (is.null(MeanAtAge))
    return(NULL)

  dd <- dim(MeanAtAge)
  if (is.null(dd)) {
    MeanAtAge <- array(MeanAtAge, dim=c(1, length(MeanAtAge), 1))
  }
  if (length(dd)==2) {
    MeanAtAge <- array(MeanAtAge, dim=c(ncol(MeanAtAge), nrow(MeanAtAge), 1))
  }
  dd <- dim(MeanAtAge)

  cli::cli_text('nsim: {.val { dd[1]}}')
  if (type=='Age')
    cli::cli_text('nAge: {.val { dd[2]}}')
  if (type=='Length')
    cli::cli_text('nClasses: {.val { dd[2]}}')
  cli::cli_text('nTS: {.val { dd[3]}}')

  if (dd[3]>1) {
    ts <- c(1, dd[3])
    ts <- c(ts[1], floor(median(ts)), ts[2])
    ts <- unique(ts)
  } else {
    ts <- 1
  }

  meanSim <- list()
  for (i in seq_along(ts)) {
    meanSim[[i]] <- apply(MeanAtAge[,,ts[i], drop=FALSE], 2, mean) |> round(round)

    val <- cli::cli_vec(meanSim[[i]], list("vec-trunc" = 10))
    if (dd[1]==1 & dd[3]==1) {
      cli::cli_text('{.val {val}}')
    }
    if (dd[1]>1 & dd[3]==1) {
      cli::cli_text('Mean over simulations: {.val {val}}')
    }

    if (dd[1]>1 & dd[3]>1) {
      if (!is.null(attributes(MeanAtAge)$TimeSteps)) {
        cli::cli_text('Time Step: {.val {attributes(MeanAtAge)$TimeSteps[i]}}')
      } else {
        cli::cli_text('Time Step: {.val {ts[i]}}')
      }

      cli::cli_text('Mean over simulations: {.val {val}}')

    }

  }
}


printPars <- function(Pars, round=2) {
  nms <- names(Pars)

  for (i in seq_along(nms)) {
    cli::cli_par()
    cli::cli_text('{.strong { nms[i]}}')

    values <- Pars[[i]]
    dd <- dim(values)
    if (length(values)==1) {
      values <- round(values, round)
      cli::cli_text('{.val { values}}')
    }
    if (length(values)==2) {
      cli::cli_text('Uniform Dist. with bounds: {.val { values}}')
    }

    if (length(values)>2) {
      cli::cli_text('nsim: {.val { dd[1]}}')
      cli::cli_text('nTS: {.val { dd[2]}}')
      meanSim <- round(apply(values, 1, mean), round)
      meanTS <- round(apply(values, 2, mean), round)
      meanSim <- cli::cli_vec(meanSim, list("vec-trunc" = 3))
      meanTS <- cli::cli_vec(meanTS, list("vec-trunc" = 3))
      if (dd[1]==1 & dd[2]==1) {
        cli::cli_text('Value: {.val {meanSim}}')
      }

      if (dd[1]>1 & dd[2]==1) {
        cli::cli_text('Value: {.val {meanSim}}')
      }

      if (dd[1]==1 & dd[2]>1) {
        cli::cli_text('Mean over time steps: {.val {meanSim}}')
      }

      if (dd[1]>1 & dd[2]>1) {
        meanSim <- cli::cli_vec(meanSim, list("vec-trunc" = 3))
        cli::cli_text('Mean over simulations: {.val {meanSim}}')
        cli::cli_text('Mean over time steps: {.val {meanTS}}')
      }

      if (!is.null(attributes(Pars)$TimeSteps))
        cli::cli_text('Time Steps: {.val {attributes(Pars)$TimeSteps}}')
    }
    cli::cli_end()
  }
}


print_errors <- function(errors) {
  if (inherits(errors, 'logical')) {
    if (length(errors)>0) {
      nms <- names(errors)
      for (i in seq_along(errors)) {
        cli::cli_alert_danger(c(nms[i], ': ', errors[[i]]))
      }
    }
  }else if (inherits(errors, 'list')) {
    for (i in seq_along(errors)) {
      object_names <- names(errors)
      if (length(errors[[i]])>0) {
        cli::cli_alert_info(' {.val {object_names[i]}}')
        nms <- names(errors[[i]])
        for (j in seq_along(errors[[i]])) {
          if (length(nms[i])>0) {
            cli::cli_alert_danger(c(nms[i], ': ', errors[[i]][[j]]))
          } else {
            cli::cli_alert_danger(errors[[i]][[j]])
          }
        }
      }
    }
  }
}

print_warnings <- function(warnings) {
  if (inherits(warnings, 'logical')) {
    if (length(warnings)>0) {
      nms <- names(warnings)
      for (i in seq_along(warnings)) {
        cli::cli_alert_warning(c(nms[i], ': ', warnings[[i]]))
      }
    }
  }else if (inherits(warnings, 'list')) {
    for (i in seq_along(warnings)) {
      object_names <- names(warnings)
      if (!is.null(object_names))
        cli::cli_alert_info(' {.val {object_names[i]}}')
      if (length(warnings[[i]])>0) {
        nms <- names(warnings[[i]])
        for (j in seq_along(warnings[[i]])) {
          cli::cli_alert_warning(c(nms[i], warnings[[i]][[j]]))
        }
      }
    }
  }
}

print_single <- function(object) {
  if (object@empty) {
    cli::cli_alert_info('Object is empty')
  } else {
    # Errors
    print_errors(object@errors)

    # Warnings
    print_warnings(object@warnings)

    # Messages

    # Status
    if (object@complete) {
      cli::cli_alert_success('Complete')
    } else {
      if (length(object@errors)>0) {
        cli::cli_alert_danger('Errors in object')
      } else if (!object@complete) {
        cli::cli_alert_danger('Object incomplete')
      }
    }
  }
}


## Internal Print Functions ----


PrintPopulating <- function(object, silent=FALSE, name=NULL, allup=FALSE) {
  if (silent)
    return(NULL)

  if (!is.null(name)) {
    cli::cli_progress_message("{cli::symbol$info} Populating {.val {name}}")
  } else {
    if (allup)
      cli::cli_progress_message("{cli::symbol$info}Populating {.val {toupper(class(object))}}")
    if (!allup)
      cli::cli_progress_message("{cli::symbol$info} Populating {.val {firstup(class(object))}}")
  }


}

PrintDonePopulating <- function(object, sb, silent=FALSE, name=NULL, allup=FALSE) {
  if (silent)
    return(NULL)

  cli::cli_progress_done(id = sb)

  if (!is.null(name)) {
    cli::cli_alert_success("Populated {.val {name}}")
  } else {
    if (allup)
      cli::cli_alert_success("Populated {.val {toupper(class(object))}}")
    if (!allup)
      cli::cli_alert_success("Populated {.val {firstup(class(object))}}")
  }

}



