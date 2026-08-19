
#' Show Methods
#'
#' Display a formatted summary of MSEtool S4 objects in the console.
#'
#' @param object An S4 object of the relevant class.
#'
#' @name show-methods
#' @aliases show,om-method
#' @aliases show,stock-method
#' @aliases show,ages-method
#' @aliases show,length-method
#' @aliases show,weight-method
#' @aliases show,naturalmortality-method
#' @aliases show,maturity-method
#' @aliases show,fecundity-method
#' @aliases show,srr-method
#' @aliases show,spatial-method
#' @aliases show,depletion-method
#' @aliases show,fleet-method
#' @aliases show,effort-method
#' @aliases show,catchability-method
#' @aliases show,selectivity-method
#' @aliases show,retention-method
#' @aliases show,discardmortality-method
#' @aliases show,obs-method
#' @aliases show,lifehistoryobs-method
#' @aliases show,exploitationobs-method
#' @aliases show,effortobs-method
#' @aliases show,catchobs-method
#' @aliases show,indicesobs-method
#' @aliases show,compobs-method
#' @aliases show,hist-method
#' @aliases show,mse-method
#' @aliases show,data-method
#' @aliases show,advice-method
#' @aliases show,popdynamics-method
#' @aliases show,perrecruit-method
#' @aliases show,refpointsMSY-method
#' @aliases show,equilibrium-method
#' @aliases show,stocktargeting-method
#' @aliases show,stocktransition-method
#' @aliases show,pm-method
#' @exportMethod show
NULL


.HasSlot <- function(object, slot) {
  slot %in% slotNames(object)
}

HelpTopic <- function(pkg, name) {
  paste0(pkg, "::", name)
}

AOrAn <- function(x) {
  ifelse(grepl("^[aeiouAEIOU]", x), "an", "a")
}

.ShowArray <- function(x, name, list_element=FALSE, show_list_element=TRUE) {
  
  if (is.null(x) || length(x) == 0) {
    cli::cli_text("{.var {name}}: ")
    return(invisible(NULL))
  }
  
  d <- dim(x)
  
  if (is.null(d)) {
    cli::cli_text("{.var {name}}: {.val {x}}")
    return(invisible(NULL))
  }
  
  dn <- dimnames(x) |> names()
  
  if (is.null(dn)) {
    if (list_element) {
      if (show_list_element)
        cli::cli_li(
        "{.var {name}}: {.val  { paste(d, collapse=' x ')} array} {.strong (Dimension names missing)}" 
        )
    } else {
      cli::cli_text(
        "{.var {name}}: {.val  { paste(d, collapse=' x ')} array} {.strong (Dimension names missing)}" 
      )  
    }
    
    return(invisible(NULL))
  } else {
    if (list_element) {
      cli::cli_li(
        "{.var {name}}: {.val  { paste( paste(d, dn), collapse=' x ') } array}" 
      )
    } else {
      cli::cli_text(
        "{.var {name}}: {.val  { paste( paste(d, dn), collapse=' x ') } array}" 
      )  
    }
    return(invisible(NULL))
  }
  
}

.ShowDataFrame <- function(x, name) {
  
  cli::cli_text("{.var { name }} {.emph data.frame}")
  nms <- colnames(x)
  cli::cli_ul()
  for (i in seq_len(ncol(x))) {
    cli::cli_li("{nms[i]}: {.val {x[,i]}}")
  }
  cli::cli_end()

}


.ShowX <- function(x, name=NULL, list_element=FALSE, show_list_element=TRUE, digits =3) {
  
  if (isS4(x)) {
    help_url <- paste0("ide:help:", HelpTopic('MSEtool', paste0(tolower(class(x)), '-class')))
    cli::cli_text("`{name}`: {AOrAn(class(x))} {.href [MSEtool::{tolower(class(x))}-class]({help_url})} object")
    return(invisible(NULL))
    
  }
  
  if (is.null(x) || !length(x) || all(is.na(x))) {
    cli::cli_text("{.var {name}}: {.emph not specified} ")  
    return(invisible(NULL))
  }
  
  if (inherits(x, 'character')) {
    if (length(x)==1 && nchar(x)<1) {
      cli::cli_text("{.var {name}}: {.emph not specified}")
      return(invisible(NULL))
    } 
    if (length(x)>1) {
      cli::cli_text("{.var {name}}: ")
      cli::cli_ul()
      for (i in seq_along(x)) {
        cli::cli_li("{.val {x[[i]]}}")
      }
      cli::cli_end()
    } else {
      cli::cli_text("{.var {name}}:  {.val {x}}")
    }
    
    return(invisible(NULL))
  }
  
  if (inherits(x, 'data.frame')) {
    .ShowDataFrame(x, name)
    return(invisible(NULL))
  }
  
  if (inherits(x, 'numeric') || inherits(x, 'integer')) {
    if (all(as.integer(x) != x, na.rm = TRUE)) {
      x <- signif(x, digits =digits )
    }
    cli::cli_text("{.var {name}}:  {.val {x}}")
    return(invisible(NULL))
  }
  
  if (inherits(x, 'logical')) {
    cli::cli_text("{.var {name}}:  {.val {x}}")
    return(invisible(NULL))
  }
  
  
  if (inherits(x, 'array')) {
    .ShowArray(x, name, list_element, show_list_element)
    return(invisible(NULL))
  }
  
  if (is.list(x)) {
    cli::cli_text(" {.var {name}}: {.val list length  {length(x)}}")
    if (show_list_element) {
      nms <- names(x)
      for (i in seq_along(x)) {
        if (!is.null(nms)) {
          nm <- nms[i]
        } else {
          nm <- ''
        }
        Recall(x[[i]], nm, list_element=TRUE, show_list_element=show_list_element)
        cli::cli_text('')  
      }
    }
    return(invisible(NULL))
  }
  
  
}

.ShowSlot <- function(object, slot, digits=3) {
  if (!.HasSlot(object, slot)) {
    return(invisible(NULL))
  }
  if (slot =='Model') {
    .ShowModel(object) 
    return(invisible(NULL))
  }
  
  if (slot =='Pars') {
    .ShowPars(object)
    return(invisible(NULL))
  }
  
  x <- slot(object, slot)
  
  .ShowX(x, slot, digits=digits)
}

.CliFn <- function(fun) {
  args <- names(formals(fun))
  cli::cli_text("{.var Model}: {.emph function with arguments: } {cli::cli_vec(args)}")
}


.ShowModel <- function(object) {
  if (!.HasSlot(object, "Model"))
    return(NULL)
  
  param_names <- names(object@Pars)
  
  fail <- FALSE
  if (is.null(object@Model) && length(param_names)) {
    chk <- try(.FindModel(object), silent=TRUE)
    if (inherits(chk, 'try-error')) {
      object@Model <- sub("^Error in \\S+ : ", "", as.character(chk))
      fail <- TRUE
    } else {
      object@Model <- chk
    }
  }
  
  if (is.character(object@Model)) {
    if (fail) {
      cli::cli_text("{.var Model}:  {object@Model}") 
    } else {
      cli::cli_text("{.var Model}:  {.help {HelpTopic('MSEtool', object@Model)}}")   
    }
    
    return(invisible(NULL))
  }
  
  if (is.function(object@Model)) {
    .CliFn(object@Model)
    return(invisible(NULL))
  }
  
  cli::cli_text("{.var Model}: ")
}


.ShowArrayP <- function(x, p ) {
  
  if (is.null(x) || length(x) == 0) {
    cli::cli_text("> {.val {p}}:")
    return(invisible(NULL))
  }
  
  d <- dim(x)
  dn <- dimnames(x) |> names()
  range_x <- range(x) |> signif(3)
  unique_x <- unique(x)
  
  if (length(unique_x)==1) {
    cli::cli_text(
      "> {.val {p}}: {.val {range_x[1]}}" 
    )
  } else {
    if (is.null(dn)) {
      cli::cli_text(
        
        "> {.val {p}}: {.emph { paste(d, collapse=' x ')}  array} {.strong (Dimension names missing)}" 
      )
    } else {
      cli::cli_text(
        "> {.val {p}}:  {.emph { paste( paste(d, dn), collapse=' x ')} array}. "
      )
    }
    
  }
  
}

.ShowPars <- function(object) {
  if (.HasSlot(object, "Pars")) {
    param_names <- names(object@Pars)
    
    cli::cli_text(  "{.var Pars}: ")
    
    if (length(param_names) > 0) {
      cli::cli_ul()
        for (p in param_names) {
          vals <- object@Pars[[p]]
          
          if (is.array(vals)) {
            .ShowArrayP(vals,p)
            
            
          } else if (length(vals)==1) {
            cli::cli_text(
              "\u2192 {.val {p}}: {.val {(vals)}}"
            )
            
          } else if (length(vals)==2) {
            cli::cli_text(
              "\u2192 {.val {p}}: Uniform Dist. with bounds {.val {range(vals)}}"
            )
          }
          
        }
      cli::cli_end()
    }
  }
  
}

.ShowObject <- function(object, name, ignore='Misc', classonly=FALSE, digits=3) {
  cli::cli_h2("A  {.help {HelpTopic('MSEtool', paste0(tolower(name),'-class'))}} Object")
  
  if (classonly)
    return(invisible(NULL))
  
  slots <- slotNames(object)
  slots <- slots[!slots%in%ignore]
  
  for (sl in slots) {
    if (sl =='Log')
      next
    .ShowSlot(object, sl, digits=digits)  
    if (sl %in% c('Model', 'TruncSD')) {
      cli::cli_text("")
    }
  }
}


setMethod("show", "om", function(object) {
  object <- UpdateObject(object)
  
  cli::cli_h2("An {.help MSEtool::om-class} Object")
  
  .ShowSlot(object, 'Name')
  
  # cli::cli_text("")
  
  # .ShowSlot(object, 'Agency')
  # .ShowSlot(object, 'Author')
  # .ShowSlot(object, 'Email')
  # .ShowSlot(object, 'Region')
  
  .ShowSlot(object, 'nSim')
  
  if (!is.null(object@Seasons) && object@Seasons > 1)
  .ShowSlot(object, 'Seasons')
  
  
  .ShowSlot(object, 'nYear')
  .ShowSlot(object, 'pYear')
  
  histYears <- Years(object,'H')
  projYears <- Years(object,'P')
  
  cli::cli_text("")
  
  cli::cli_text("Historical Years: {.val { paste(range(histYears), collapse = ' - ')} ({length(histYears)})}")
  cli::cli_text("Projection Years: {.val { paste(range(projYears), collapse = ' - ')} ({length(projYears)})}")
  
  cli::cli_text("")
  
  MissingSlots <- CheckOM(object)
  
  # Stock
  if (is.null(object@Stock)) {
    cli::cli_text("Stocks: {.emph None specified}")
  } else {
    stockNames <- StockNames(object)  
    cli::cli_text("Stock{?s}: {.val {stockNames}}")
  }
  
  if (is.null(object@Fleet)) {
    cli::cli_text("Fleets: {.emph None specified}")
  } else {
    fleetNames <- FleetNames(object)  
    cli::cli_text("Fleet{?s}: {.val {fleetNames}}")
  }
  
  if (!is.null(object@Stock)) {
    MissingStock <- MissingSlots$MissingStock
    
    if (!is.null(MissingStock)) {
      if (!is.list(MissingStock)) {
        MissingStock <- list(MissingStock)
        names(MissingStock) <- stockNames
      }
      if (lapply(MissingStock, length) |> unlist() |> max() != 0) {
        cli::cli_text('')
        cli::cli_alert_danger('Missing Required Slots:')
        for (i in seq_along(MissingStock)) {
          if (!is.null(MissingStock[[i]])) {
            cli::cli_alert("Stock: {.val {stockNames[i]}}")
            for (j in seq_along(MissingStock[[i]])) {
              cli::cli_li("Slot: {.val {MissingStock[[i]][[j]]}}")
            }
          }
        }
      }
    }
  }
  
  if (!is.null(object@Fleet)) {
    MissingFleet <- MissingSlots$MissingFleet
    
    if (!is.null(MissingFleet)) {
      if (!is.list(MissingFleet)) {
        MissingFleet <- list(list(MissingFleet))
        names(MissingFleet) <- stockNames
        names(MissingFleet[[1]]) <- fleetNames
        
      }
      if ( lapply(MissingFleet, lapply, length)|> unlist() |> max() != 0) {
        cli::cli_text('')
        cli::cli_alert_danger('Missing Required Slots:')
        for (i in seq_along(MissingFleet)) {
          if (!is.null(MissingFleet[[i]])) {
            cli::cli_alert("Stock: {.val {stockNames[i]}}")
            for (j in seq_along(MissingFleet[[i]])) {
              cli::cli_alert("Fleet: {.val {fleetNames[j]}}")
              for (k in seq_along(MissingFleet[[i]][[j]]))
                cli::cli_li("Slot: {.val {MissingFleet[[i]][[j]][[k]]}}")
            }
          }
        }
      }
    }
  }

  cli::cli_text("")
  .CheckLog(object, 'OM')
})


setMethod('show', 'stock', function(object) {
  object <- UpdateObject(object)
  cli::cli_h2("A {.help MSEtool::stock-class} Object")
  
  .ShowSlot(object, 'Name')
  
  .ShowSlot(object, 'CommonName')
  .ShowSlot(object, 'Species')
  
  cli::cli_text("")
  
  
  slots <- c('Ages',
             'Length',
             'Weight',
             'NaturalMortality',
             'Maturity',
             'Fecundity',
             'SRR',
             'Spatial',
             'Depletion'
  )
  for (name in slots) {
    if (isNewObject(slot(object, name))) {
      cli::cli_text("{.var {name}}: {.emph not specified}")
    } else {
      
      cli::cli_text("{.var {name}}: {AOrAn(name)}  {.help {HelpTopic('MSEtool', name)}} Object")
    }
  }

})

## ---- Ages ----
setMethod("show", "ages", function(object) {
  
  cli::cli_h2("An {.help MSEtool::ages-class} Object")
  
  .ShowSlot(object, 'MinAge')
  .ShowSlot(object, 'MaxAge')
  .ShowSlot(object, 'Units')
  .ShowSlot(object, 'PlusGroup')
  
  AgeClasses <- CalcAgeClasses(object) 
  if (!is.null(AgeClasses)) {
    if (isTRUE(object@PlusGroup)) {
      AgeClasses[length(AgeClasses)] <- paste0(AgeClasses[length(AgeClasses)], "+")
    } 
  }
  cli::cli_text("{.var Classes}: {.val {AgeClasses}}")
})


setMethod("show", "length", function(object) {
  .ShowObject(object, 'Length')
})

setMethod("show", "weight", function(object) {
  .ShowObject(object, 'Weight')
})

setMethod("show", "naturalmortality", function(object) {
  .ShowObject(object, 'NaturalMortality')
})

setMethod("show", "maturity", function(object) {
  .ShowObject(object, 'Maturity')
})

setMethod("show", "fecundity", function(object) {
  .ShowObject(object, 'Fecundity')
})

setMethod("show", "srr", function(object) {
  .ShowObject(object, 'SRR')
})

setMethod("show", "spatial", function(object) {
  .ShowObject(object, 'Spatial')
})

setMethod("show", "depletion", function(object) {
  .ShowObject(object, 'Depletion')
})


setMethod('show', 'fleet', function(object) {
  object <- UpdateObject(object)
  cli::cli_h2("A {.help MSEtool::fleet-class} Object")
  
  .ShowSlot(object, 'Name')
  
  cli::cli_text("")
  
  slots <- c('Effort',
             'Catchability',
             'Selectivity',
             'Retention',
             'DiscardMortality'
  )
  for (name in slots) {
    if (isNewObject(slot(object, name))) {
      cli::cli_text("`{name}`: {.emph not specified}")
    } else {
      cli::cli_text("`{name}`: {AOrAn(name)} {.help {HelpTopic('MSEtool', name)}} Object")
    }
  }
  
  .ShowSlot(object, 'Closure')
  .ShowSlot(object, 'WeightFleetRetained')
  .ShowSlot(object, 'WeightFleetSelected')

})

setMethod("show", "effort", function(object) {
  .ShowObject(object, 'Effort')
})

setMethod("show", "catchability", function(object) {
  .ShowObject(object, 'Catchability')
})

setMethod("show", "selectivity", function(object) {
  .ShowObject(object, 'Selectivity')
})

setMethod("show", "retention", function(object) {
  .ShowObject(object, 'Retention')
})

setMethod("show", "discardmortality", function(object) {
  .ShowObject(object, 'DiscardMortality')
})


setMethod('show', 'obs', function(object) {
  cli::cli_h2("An {.help MSEtool::obs-class} Object")
  
  .ShowSlot(object, 'Name')
  
  cli::cli_text("")
  
  slot_constructor <- c(
    LifeHistory    = 'LifeHistoryObs',
    Exploitation   = 'ExploitationObs',
    Effort         = 'EffortObs',
    Landings       = 'CatchObs',
    Discards       = 'CatchObs',
    CPUE           = 'IndicesObs',
    Survey         = 'IndicesObs',
    LandingsAtAge  = 'CompObs',
    DiscardsAtAge  = 'CompObs',
    LandingsAtSize = 'CompObs',
    DiscardsAtSize = 'CompObs'
  )
  
  for (name in names(slot_constructor)) {
    constructor <- slot_constructor[[name]]
    if (isNewObject(slot(object, name))) {
      cli::cli_text("`{name}`: {.emph not specified}")
    } else {
      cli::cli_text("`{name}`: {AOrAn(constructor)} {.help {HelpTopic('MSEtool', constructor)}} Object")
    }
  }
})


# TODO
setMethod('show', 'lifehistoryobs', function(object) {
  cli::cli_h2("A {.help MSEtool::lifehistoryobs-class} Object")
  cli::cli_text("{.emph Note: This class is a placeholder and is not currently populated.}")
  cli::cli_text("")
  
  slots <- c('Ages', 'Length', 'Weight', 'NaturalMortality', 
             'Maturity', 'Fecundity', 'SRR', 'Spatial', 'Depletion')
  
  for (name in slots) {
    x <- slot(object, name)
    if (length(x) == 0) {
      cli::cli_text("{.var {name}}: {.emph not specified}")
    } else {
      cli::cli_text("{.var {name}}: {.val list length {length(x)}}")
    }
  }
})

# TODO
setMethod('show', 'exploitationobs', function(object) {
  cli::cli_h2("An {.help MSEtool::exploitationobs-class} Object")
  cli::cli_text("{.emph Note: This class is a placeholder and is not currently populated.}")
  cli::cli_text("")
  
  slots <- c('Selectivity', 'Retention', 'DiscardMortality')
  
  for (name in slots) {
    x <- slot(object, name)
    if (length(x) == 0) {
      cli::cli_text("{.var {name}}: {.emph not specified}")
    } else {
      cli::cli_text("{.var {name}}: {.val list length {length(x)}}")
    }
  }
})

setMethod('show', 'effortobs', function(object) {
  .ShowObject(object, 'effortobs')
})

setMethod('show', 'catchobs', function(object) {
  .ShowObject(object, 'catchobs')
})

setMethod('show', 'indicesobs', function(object) {
  .ShowObject(object, 'indicesobs')
})

setMethod('show', 'compobs', function(object) {
  .ShowObject(object, 'compobs')
})


setMethod('show', 'imp', function(object) {
  object <- UpdateObject(object)
  cli::cli_h2("An {.help MSEtool::imp-class} Object")

  .ShowSlot(object, 'Name')

  cli::cli_text("")

  slots <- c('TAC', 'Effort', 'Size')
  for (name in slots) {
    if (isNewObject(slot(object, name))) {
      cli::cli_text("`{name}`: {.emph not specified}")
    } else {
      cli::cli_text("`{name}`: {AOrAn(name)} {.help {HelpTopic('MSEtool', 'impslot-class')}} Object")
    }
  }
})

setMethod("show", "impslot", function(object) {
  .ShowObject(object, 'ImpSlot')
})


setMethod('show', 'hist', function(object) {
  cli::cli_h2("A {.help MSEtool::hist-class} Object")
  cli::cli_text("")
  
  slots <- slotNames(object)
  classslots <- c('OM', 'Unfished', 'Reference')
  
  for (name in classslots) {
      cli::cli_text("`{name}`: {AOrAn(name)} {.help {HelpTopic('MSEtool', paste0(tolower(name), '-class'))}} Object")
    
  }
  cli::cli_text("")
  
  slots2 <- slots[!slots %in% classslots]
  
  for (sl in slots2) {
    if (sl == 'Data') {
      # TODO
      next
    }
    if (sl == 'Log') {
      next
    }
    if (sl == 'Misc') {
      next
    }
    .ShowX(slot(object, sl), sl, show_list_element=FALSE)
  }

  cli::cli_text("")
  .CheckLog(object, 'Hist')


  # cli::cli_text("{.var OM}: A {.help MSEtool::OM} Object")
  # cli::cli_text("{.var Unfished}: A {.help MSEtool::unfished-class} Object")
  #
  # slots %in% c('OM', 'Unfished')
  #
  # cli::cli_text("Slots:")
  # cli::cli_li(
  # slotNames(object))

})


setMethod('show', 'mse', function(object) {
  cli::cli_h2("A {.help MSEtool::mse-class} Object")
  
  .ShowSlot(object@OM, 'Name')
  .ShowSlot(object@OM, 'nSim')
  
  if (!is.null(object@OM@Seasons) && object@OM@Seasons > 1)
    .ShowSlot(object@OM, 'Seasons')
  
  cli::cli_text('`MPs`: {.val {names(object@MPs)}}')
  histYears <- Years(object@OM,'H')
  projYears <- Years(object@OM,'P')
  
  cli::cli_text("")
  
  cli::cli_text("Historical Years: {.val { paste(range(histYears), collapse = ' - ')} ({length(histYears)})}")
  cli::cli_text("Projection Years: {.val { paste(range(projYears), collapse = ' - ')} ({length(projYears)})}")

  cli::cli_text("")

  .CheckLog(object, 'MSE')

})


setMethod('show', 'data', function(object) {
  .ShowObject(object, 'data')
  
  # cli::cli_h2("A {.help MSEtool::data-class} Object")
  # cli::cli_text("")
  # 
  # cli::cli_text("Slots:")
  # cli::cli_li(
  #   slotNames(object))
})


setMethod('show', 'advice', function(object) {
  .ShowObject(object, 'Advice', digits=10)
})


setMethod('show', 'perrecruit', function(object) {
  .ShowObject(object, 'perrecruit')
})

setMethod('show', 'refpointsMSY', function(object) {
  .ShowObject(object, 'refpointsMSY')
})

setMethod('show', 'equilibrium', function(object) {
  .ShowObject(object, 'equilibrium')
})

setMethod('show', 'reference', function(object) {
  .ShowObject(object, 'reference')
})


setMethod('show', 'stocktargeting', function(object) {
  .ShowObject(object, 'stocktargeting')
})


setMethod('show', 'stocktransition', function(object) {
  cli::cli_h2("A {.help MSEtool::stocktransition-class} Object")

  cli::cli_text("`From`: {.val {object@From}}")
  cli::cli_text("`To`: {.val {object@To}}")

  cli::cli_text("")

  if (is.null(object@Frac)) {
    cli::cli_text("`Frac`: {.emph not specified}")
    return(invisible(NULL))
  }

  d  <- dim(object@Frac)
  dn <- dimnames(object@Frac)
  ages <- if (!is.null(dn) && !is.null(dn$Age)) as.numeric(dn$Age) else seq_len(d[2]) - 1

  cli::cli_text("`Frac`: {.val {paste(d, collapse = ' x ')}} array ({.val {paste(names(dn), collapse = ', ')}})")
  cli::cli_text("Age range: {.val {range(ages)}}")
})


setMethod('show', 'popdynamics', function(object) {
  .ShowObject(object, 'popdynamics')
})


setMethod('show', 'pm', function(object) {
  cli::cli_h2("A {.help MSEtool::pm-class} Object")

  .ShowSlot(object, 'Name')
  .ShowSlot(object, 'Caption')

  if (!all(is.na(object@Ref)))
    .ShowSlot(object, 'Ref')

  cli::cli_text("`MPs`: {.val {object@MPs}}")

  if (length(object@Years))
    cli::cli_text("`Years`: {.val {paste(range(object@Years), collapse = ' - ')} ({length(object@Years)})}")

  cli::cli_text("")

  Mean <- object@Mean
  if (length(Mean) && !all(is.na(Mean))) {
    group_nm <- names(dimnames(Mean))[1]
    if (is.null(group_nm))
      group_nm <- 'Stock'
    cli::cli_text("{.strong {group_nm} x MP} mean:")
    print(round(Mean, 3))
  }
})
