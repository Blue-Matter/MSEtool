#' Generic show method
#' 
#'
#' @param object Object to print to console
#' @importFrom methods show
#' @export
show <- function(object) methods::show(object)



hasSlot <- function(object, slot) {
  slot %in% slotNames(object)
}

help_topic <- function(pkg, name) {
  paste0(pkg, "::", name)
}

a_or_an <- function(x) {
  ifelse(grepl("^[aeiouAEIOU]", x), "an", "a")
}

.show_array <- function(x, name) {
  
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
    cli::cli_text(
      "{.var {name}}: {.val  { paste(d, collapse=' x ')} array} {.strong (Dimension names missing)}" 
    )
    return(invisible(NULL))
  } else {
    cli::cli_text(
      "{.var {name}}: {.val  { paste( paste(d, dn), collapse=' x ') } array}" 
    )
    return(invisible(NULL))
  }
  
}

.show_data_frame <- function(x, name) {
  
  cli::cli_text("{.var { name }} {.emph data.frame}")
  nms <- colnames(x)
  cli::cli_ul()
  for (i in seq_len(nrow(x))) {
    cli::cli_li("{nms[i]}: {.val {x[,i]}}")
  }
  cli::cli_end()

}


.show_x <- function(x, name=NULL) {
  
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
  }
  
  if (inherits(x, 'numeric')) {
    if (all(as.integer(x) != x)) {
      x <- signif(x,3)
    }
    cli::cli_text("{.var {name}}:  {.val {x}}")
  }
  
  if (inherits(x, 'logical')) {
    cli::cli_text("{.var {name}}:  {.val {x}}")
  }
  
  
  if (inherits(x, 'array')) {
    .show_array(x, name)
  }
  
  if (is.list(x)) {
    cli::cli_text(" {name}: {.emph list length {.val {length(x)}}}")
    nms <- names(x)
    for (i in seq_along(x)) {
      if (!is.null(nms)) {
        nm <- nms[i]
      } else {
        nm <- ''
      }
      Recall(x[[i]], nm)
      cli::cli_text('')
    }
    
  }
  
}

.show_slot <- function(object, slot) {
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
  
  .show_x(x, slot)
}

cli_fn <- function(fun) {
  args <- names(formals(fun))
  cli::cli_text("{.var Model}: {.emph function with arguments: } {cli::cli_vec(args)}")
}


.show_model <- function(object) {
  if (!hasSlot(object, "Model"))
    return(NULL)
  
  param_names <- names(object@Pars)
  
  if (is.null(object@Model) && length(param_names)) {
    object@Model <- FindModel(object)
  
  }
  
  if (is.character(object@Model)) {
    cli::cli_text("{.var Model}:  {.help {help_topic('MSEtool', object@Model)}}") 
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
    cli::cli_text("→ {.val {p}}:")
    return(invisible(NULL))
  }
  
  d <- dim(x)
  dn <- dimnames(x) |> names()
  range_x <- range(x) |> signif(3)
  unique_x <- unique(x)
  
  if (length(unique_x)==1) {
    cli::cli_text(
      "→ {.val {p}}: {.val {range_x[1]}}" 
    )
  } else {
    if (is.null(dn)) {
      cli::cli_text(
        
        "→ {.val {p}}: {.emph { paste(d, collapse=' x ')} array}} {.strong (Dimension names missing)}" 
      )
    } else {
      cli::cli_text(
        "→ {.val {p}}:  {.emph { paste( paste(d, dn), collapse=' x ')} array}. "
      )
    }
    
  }
  
}

.show_pars <- function(object) {
  if (hasSlot(object, "Pars")) {
    param_names <- names(object@Pars)
    
    cli::cli_text(  "{.var Pars}: ")
    
    if (length(param_names) > 0) {
      cli::cli_ul(
        for (p in param_names) {
          vals <- object@Pars[[p]]
          
          if (is.array(vals)) {
            .show_array_p(vals,p)
            
            
          } else if (length(vals)==1) {
            cli::cli_text(
              "→ {.val {p}}: {.val {(vals)}}"
              
            )
            
          } else if (length(vals)==2) {
            cli::cli_text(
              "→ {.val {p}}: Uniform Dist. with bounds {.val {range(vals)}}"
            )
          }
          
        }
      )
    }
  }
  
}

.show_object <- function(object, name, ignore='Misc') {
  cli::cli_h2("A  {.help {help_topic('MSEtool', name)}} Object")
  
  slots <- slotNames(object)
  slots <- slots[!slots%in%ignore]
  
  for (sl in slots) {
    .show_slot(object, sl)  
    if (sl %in% c('Model', 'TruncSD')) {
      cli::cli_text("")
    }
  }
}




# ---- OM ----

setMethod("show", "om", function(object) {
  
  cli::cli_h2("An {.help MSEtool::OM} Object")
  
  .show_slot(object, 'Name')
  
  # cli::cli_text("")
  
  # .show_slot(object, 'Agency')
  # .show_slot(object, 'Author')
  # .show_slot(object, 'Email')
  # .show_slot(object, 'Region')
  
  .show_slot(object, 'nSim')
  
  .show_slot(object, 'CurrentYear')
  .show_slot(object, 'Seasons')
  
  .show_slot(object, 'nYear')
  .show_slot(object, 'pYear')
  histYears <- Years(object,'H')
  projYears <- Years(object,'P')
  
  cli::cli_text("")
  
  cli::cli_text("Historical Years: {.val { paste(range(histYears), collapse = ' - ')}}")
  cli::cli_text("Projection Years: {.val { paste(range(projYears), collapse = ' - ')}}")
  
  cli::cli_text("")
  
  stockNames <- StockNames(object)
  fleetNames <- FleetNames(object)
  cli::cli_text("Stocks: {.val {stockNames}}")
  .show_x(fleetNames, 'Fleets')
  # cli::cli_text("Fleets: {.val {FleetNames(object)}}")
  
})

# ---- Stock ----

setMethod('show', 'stock', function(object) {
  
  cli::cli_h2("A {.help MSEtool::Stock} Object")
  
  .show_slot(object, 'Name')
  
  cli::cli_text("")
  
  .show_slot(object, 'CommonName')
  .show_slot(object, 'Species')
  
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
      cli::cli_text("{.strong {name}}: {.emph not specified}")
    } else {
      
      cli::cli_text("{.strong {name}}: {a_or_an(name)}  {.help {help_topic('MSEtool', name)}} Object")
    }
  }

})


setMethod("show", "ages", function(object) {
  
  cli::cli_h2("An {.help MSEtool::Ages} Object")
  
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
  cli::cli_text("{.strong Classes}: {.val {AgeClasses}}")
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
  
  cli::cli_h2("A {.help MSEtool::Fleet} Object")
  
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
      cli::cli_text("{.strong {name}}: {.emph not specified}")
    } else {
      
      cli::cli_text("{.strong {name}}: {a_or_an(name)}  {.help {help_topic('MSEtool', name)}} Object")
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


# ---- Hist ----

#' @rdname show
setMethod('show', 'hist', function(object) {
  cli::cli_h2("A {.help MSEtool::Hist} Object")
  cli::cli_text("...")
})




# ---- MSE ----

#' @rdname show
setMethod('show', 'mse', function(object) {
  cli::cli_h2("A {.help MSEtool::MSE} Object")
  cli::cli_text("...")
})



# ---- Data ----

#' @rdname show
setMethod('show', 'data', function(object) {
  cli::cli_h2("A {.help MSEtool::Data} Object")
  cli::cli_text("...")
})


# ---- Advice ----

#' @rdname show
setMethod('show', 'advice', function(object) {
  cli::cli_h2("A {.help MSEtool::Advice} Object")
  cli::cli_text("...")
})



# ---- popdynamics ----

#' @rdname show
setMethod('show', 'popdynamics', function(object) {
  .show_object(object, 'popdynamics')
  
})











