
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
#' @exportMethod show
NULL


hasSlot <- function(object, slot) {
  slot %in% slotNames(object)
}

help_topic <- function(pkg, name) {
  paste0(pkg, "::", name)
}

a_or_an <- function(x) {
  ifelse(grepl("^[aeiouAEIOU]", x), "an", "a")
}

.show_array <- function(x, name, list_element=FALSE, show_list_element=TRUE) {
  
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

.show_data_frame <- function(x, name) {
  
  cli::cli_text("{.var { name }} {.emph data.frame}")
  nms <- colnames(x)
  cli::cli_ul()
  for (i in seq_len(ncol(x))) {
    cli::cli_li("{nms[i]}: {.val {x[,i]}}")
  }
  cli::cli_end()

}


.show_x <- function(x, name=NULL, list_element=FALSE, show_list_element=TRUE, digits =3) {
  
  if (isS4(x)) {
    help_url <- paste0("ide:help:", help_topic('MSEtool', paste0(tolower(name), '-class')))
    cli::cli_text("`{name}`: {a_or_an(class(x))} {.href [MSEtool::{tolower(class(x))}-class]({help_url})} object")
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
    .show_data_frame(x, name)
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
    .show_array(x, name, list_element, show_list_element)
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

.show_slot <- function(object, slot, digits=3) {
  if (!hasSlot(object, slot)) {
    return(invisible(NULL))
  }
  if (slot =='Model') {
    .show_model(object) 
    return(invisible(NULL))
  }
  
  if (slot =='Pars') {
    .show_pars(object)
    return(invisible(NULL))
  }
  
  x <- slot(object, slot)
  
  .show_x(x, slot, digits=digits)
}

cli_fn <- function(fun) {
  args <- names(formals(fun))
  cli::cli_text("{.var Model}: {.emph function with arguments: } {cli::cli_vec(args)}")
}


.show_model <- function(object) {
  if (!hasSlot(object, "Model"))
    return(NULL)
  
  param_names <- names(object@Pars)
  
  fail <- FALSE
  if (is.null(object@Model) && length(param_names)) {
    chk <- try(FindModel(object), silent=TRUE)
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
      cli::cli_text("{.var Model}:  {.help {help_topic('MSEtool', object@Model)}}")   
    }
    
    return(invisible(NULL))
  }
  
  if (is.function(object@Model)) {
    cli_fn(object@Model)
    return(invisible(NULL))
  }
  
  cli::cli_text("{.var Model}: ")
}


.show_array_p <- function(x, p ) {
  
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

.show_pars <- function(object) {
  if (hasSlot(object, "Pars")) {
    param_names <- names(object@Pars)
    
    cli::cli_text(  "{.var Pars}: ")
    
    if (length(param_names) > 0) {
      cli::cli_ul()
        for (p in param_names) {
          vals <- object@Pars[[p]]
          
          if (is.array(vals)) {
            .show_array_p(vals,p)
            
            
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

.show_object <- function(object, name, ignore='Misc', classonly=FALSE, digits=3) {
  cli::cli_h2("A  {.help {help_topic('MSEtool', paste0(tolower(name),'-class'))}} Object")
  
  if (classonly)
    return(invisible(NULL))
  
  slots <- slotNames(object)
  slots <- slots[!slots%in%ignore]
  
  for (sl in slots) {
    if (sl =='Log')
      next
    .show_slot(object, sl, digits=digits)  
    if (sl %in% c('Model', 'TruncSD')) {
      cli::cli_text("")
    }
  }
}




# ---- OM ----

setMethod("show", "om", function(object) {
  object <- UpdateObject(object)
  
  cli::cli_h2("An {.help MSEtool::om-class} Object")
  
  .show_slot(object, 'Name')
  
  # cli::cli_text("")
  
  # .show_slot(object, 'Agency')
  # .show_slot(object, 'Author')
  # .show_slot(object, 'Email')
  # .show_slot(object, 'Region')
  
  .show_slot(object, 'nSim')
  
  if (!is.null(object@Seasons) && object@Seasons > 1)
  .show_slot(object, 'Seasons')
  
  
  .show_slot(object, 'nYear')
  .show_slot(object, 'pYear')
  
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
})



# ---- Stock ----

setMethod('show', 'stock', function(object) {
  object <- UpdateObject(object)
  cli::cli_h2("A {.help MSEtool::stock-class} Object")
  
  .show_slot(object, 'Name')
  
  .show_slot(object, 'CommonName')
  .show_slot(object, 'Species')
  
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
      
      cli::cli_text("{.var {name}}: {a_or_an(name)}  {.help {help_topic('MSEtool', name)}} Object")
    }
  }

})

## ---- Ages ----
setMethod("show", "ages", function(object) {
  
  cli::cli_h2("An {.help MSEtool::ages-class} Object")
  
  .show_slot(object, 'MinAge')
  .show_slot(object, 'MaxAge')
  .show_slot(object, 'Units')
  .show_slot(object, 'PlusGroup')
  
  AgeClasses <- CalcAgeClasses(object) 
  if (!is.null(AgeClasses)) {
    if (isTRUE(object@PlusGroup)) {
      AgeClasses[length(AgeClasses)] <- paste0(AgeClasses[length(AgeClasses)], "+")
    } 
  }
  cli::cli_text("{.var Classes}: {.val {AgeClasses}}")
})


setMethod("show", "length", function(object) {
  .show_object(object, 'Length')
})

setMethod("show", "weight", function(object) {
  .show_object(object, 'Weight')
})

setMethod("show", "naturalmortality", function(object) {
  .show_object(object, 'NaturalMortality')
})

setMethod("show", "maturity", function(object) {
  .show_object(object, 'Maturity')
})

setMethod("show", "fecundity", function(object) {
  .show_object(object, 'Fecundity')
})

setMethod("show", "srr", function(object) {
  .show_object(object, 'SRR')
})

setMethod("show", "spatial", function(object) {
  .show_object(object, 'Spatial')
})

setMethod("show", "depletion", function(object) {
  .show_object(object, 'Depletion')
})

# ---- Fleet ----

setMethod('show', 'fleet', function(object) {
  object <- UpdateObject(object)
  cli::cli_h2("A {.help MSEtool::fleet-class} Object")
  
  .show_slot(object, 'Name')
  
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
      cli::cli_text("`{name}`: {a_or_an(name)} {.help {help_topic('MSEtool', name)}} Object")
    }
  }
  
  .show_slot(object, 'Closure')
  .show_slot(object, 'WeightFleet')
  
})

setMethod("show", "effort", function(object) {
  .show_object(object, 'Effort')
})

setMethod("show", "catchability", function(object) {
  .show_object(object, 'Catchability')
})

setMethod("show", "selectivity", function(object) {
  .show_object(object, 'Selectivity')
})

setMethod("show", "retention", function(object) {
  .show_object(object, 'Retention')
})

setMethod("show", "discardmortality", function(object) {
  .show_object(object, 'DiscardMortality')
})

# ---- Obs ----

setMethod('show', 'obs', function(object) {
  cli::cli_h2("An {.help MSEtool::obs-class} Object")
  
  .show_slot(object, 'Name')
  
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
      cli::cli_text("`{name}`: {a_or_an(constructor)} {.help {help_topic('MSEtool', constructor)}} Object")
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
  .show_object(object, 'effortobs')
})

setMethod('show', 'catchobs', function(object) {
  .show_object(object, 'catchobs')
})

setMethod('show', 'indicesobs', function(object) {
  .show_object(object, 'indicesobs')
})

setMethod('show', 'compobs', function(object) {
  .show_object(object, 'compobs')
})

# ---- Hist ----

setMethod('show', 'hist', function(object) {
  cli::cli_h2("A {.help MSEtool::hist-class} Object")
  cli::cli_text("")
  
  slots <- slotNames(object)
  classslots <- c('OM', 'Unfished', 'Reference')
  
  for (name in classslots) {
      cli::cli_text("`{name}`: {a_or_an(name)} {.help {help_topic('MSEtool', paste0(tolower(name), '-class'))}} Object")
    
  }
  cli::cli_text("")
  
  slots2 <- slots[!slots %in% classslots]
  
  for (sl in slots2) {
    if (sl == 'Data') {
      # TODO
      next
    }
    if (sl == 'Log') {
      # TODO
      next
    }
    if (sl == 'Misc') {
      next
    }
    .show_x(slot(object, sl), sl, show_list_element=FALSE)
  }
  
  
  
  
  # cli::cli_text("{.var OM}: A {.help MSEtool::OM} Object")
  # cli::cli_text("{.var Unfished}: A {.help MSEtool::unfished-class} Object")
  # 
  # slots %in% c('OM', 'Unfished')
  # 
  # cli::cli_text("Slots:")
  # cli::cli_li(
  # slotNames(object))
 
})




# ---- MSE ----

setMethod('show', 'mse', function(object) {
  cli::cli_h2("A {.help MSEtool::mse-class} Object")
  
  .show_slot(object@OM, 'Name')
  .show_slot(object@OM, 'nSim')
  
  if (!is.null(object@OM@Seasons) && object@OM@Seasons > 1)
    .show_slot(object@OM, 'Seasons')
  
  cli::cli_text('`MPs`: {.val {names(object@MPs)}}')
  histYears <- Years(object@OM,'H')
  projYears <- Years(object@OM,'P')
  
  cli::cli_text("")
  
  cli::cli_text("Historical Years: {.val { paste(range(histYears), collapse = ' - ')} ({length(histYears)})}")
  cli::cli_text("Projection Years: {.val { paste(range(projYears), collapse = ' - ')} ({length(projYears)})}")
  
  cli::cli_text("")
  
})



# ---- Data ----

setMethod('show', 'data', function(object) {
  .show_object(object, 'data')
  
  # cli::cli_h2("A {.help MSEtool::data-class} Object")
  # cli::cli_text("")
  # 
  # cli::cli_text("Slots:")
  # cli::cli_li(
  #   slotNames(object))
})


# ---- Advice ----


setMethod('show', 'advice', function(object) {
  .show_object(object, 'Advice', digits=10)
})


# ---- per-recruit ----
setMethod('show', 'perrecruit', function(object) {
  .show_object(object, 'perrecruit')
  
})

# ---- per-recruit ----
setMethod('show', 'refpointsMSY', function(object) {
  .show_object(object, 'refpointsMSY')
  
})

# ---- popdynamics ----


setMethod('show', 'popdynamics', function(object) {
  .show_object(object, 'popdynamics')
  
})











