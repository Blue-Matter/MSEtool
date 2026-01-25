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

.show_model <- function(object) {
  if (!hasSlot(object, "Model"))
    return(NULL)
  
  param_names <- names(object@Pars)
  
  if (is.null(object@Model) && length(param_names)) {
    object@Model <- FindModel(object)
  }
   
  
  if (is.null(object@Model)) {
    cli::cli_text("Model: {.emph not specified}")
  } else {
    cli::cli_text("Model: {.val {object@Model}}") 
  }
 
}


  
.show_array_p <- function(x, p ) {
  
  if (is.null(x) || length(x) == 0) {
    cli::cli_text("→ {.val {p}}: {.emph not specified}")
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
        
        "→ {.val {p}}: {.emph {.val {range_x}}. (Range { paste(d, collapse=' x ')} array}} {.strong (Dimension names missing)}" 
      )
    } else {
      cli::cli_text(
        "→ {.val {p}}:  {.emph { paste( paste(d, dn), collapse=' x ')} array}. Range: {.val {range_x}}"
      )
    }
   
  }

}

.show_array <- function(x, name, var='Age') {
  
  if (is.null(x) || length(x) == 0) {
    cli::cli_text("{name}: {.emph not specified}")
    return(invisible(NULL))
  }
  
  d <- dim(x)

  
  if (is.null(d)) {
    cli::cli_text("{name}: {.val {x}}")
    return(invisible(NULL))
  }
  
  dn <- dimnames(x) |> names()
  mean_x <- apply(x, var, mean) |> signif(3)
  unique_x <- unique(x)
  
  if (length(unique_x)==1) {
    cli::cli_text("{name}: {.val {mean_x}}" )
  } else {
    if (is.null(dn)) {
      cli::cli_text(
        "{name}: {.emph  { paste(d, collapse=' x ')} array} {.strong (Dimension names missing)}" 
        
        # "{name}: {.val {mean_x}}  {.emph  Mean over {dn[dn!=var]} of { paste(d, collapse=' x ')} array} {.strong (Dimension names missing)}" 
      )
    } else {
      cli::cli_text(
        "{name}: {.emph  { paste( paste(d, dn), collapse=' x ') } array}"
        # "{name}: {.val {mean_x}}  {.emph  Mean over {dn[dn!=var]} of { paste( paste(d, dn), collapse=' x ')} array}" 
      )
    }
  }
  
  
  
}

.show_pars <- function(object) {
  if (hasSlot(object, "Pars")) {
    param_names <- names(object@Pars)
    
    cli::cli_text(
      if (length(param_names) == 0) {
        "Pars: {.emph not specified}"
      } else {
        "Pars:"
      }
    )
    
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


.show_units <- function(object) {
  if (!hasSlot(object, "Units"))
    return(NULL)
  cli::cli_text("Units: {.val {object@Units}}")
}


.show_mean_at_ <- function(object, name='MeanAtAge', var='Age') {
  if (!hasSlot(object, name))
    return(NULL)
  
  if (is.null(slot(object, name))) {
    cli::cli_text("{name}: {.emph not specified}")
  } else {
    .show_array(slot(object, name), name, var)
  }
}

.show_slot <- function(object, slot) {
  if (!hasSlot(object, slot)) {
    return(invisible(NULL))
  }
  
  val <- slot(object, slot)
  if (is.null(val) || !length(val)) {
    cli::cli_text("{slot}:  {.emph not specified}")
  } else {
    if (is.numeric(val) && as.integer(val) != val) {
      val <- signif(val,3)
    }
    cli::cli_text("{slot}:  {.val {val}}")
  }
}

.show_object <- function(object) {
  

  .show_model(object)
  .show_pars(object)
  .show_units(object)
  .show_mean_at_(object)
  .show_mean_at_(object, 'CVatAge')
  
  .show_slot(object, 'Dist')
  .show_slot(object, 'TruncSD')
  
  .show_mean_at_(object, 'MeanAtLength', 'Class')
  .show_mean_at_(object, 'MeanAtWeight', 'Class')
  
  .show_slot(object, 'Timing')
  .show_slot(object, 'Classes')
 
  .show_slot(object, 'Semelparous')
  
  # if (hasSlot(object, 'ASK'))  {
  #   if (inherits(object, 'length')) {
  #     cli::cli_text("ALK:", object@ASK)
  #   } else if  (inherits(object, 'weight')) {
  #     cli::cli_text("AWK:", object@ASK)
  #   }
  # }
    
  
  
}




# ---- MSE ----

#' @rdname show
setMethod('show', 'mse', function(object) {
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::MSE} Object")
  cli::cli_text("...")
})


# ---- Hist ----

#' @rdname show
setMethod('show', 'hist', function(object) {
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Hist} Object")
  cli::cli_text("...")
})

# ---- Data ----

#' @rdname show
setMethod('show', 'data', function(object) {
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Data} Object")
  cli::cli_text("...")
})


# ---- Stock ----

















## --- OM  ----


## --- CheckList Object ----

# Clashes with Slick - need new object class name

# #' @describeIn show Print a `CheckList` object
# setMethod('show', 'CheckList', function(object) {
# 
#   cli::cli_h3('Checking')
# 
#   if (is.list(object@empty)) {
#     print_list(object)
#   } else {
#     print_single(object)
#   }
# })
# 
# 


## --- Supporting Functions ----


printASK <- function(ASK) {
  if (is.null(ASK))
    return(NULL)
  dd <- dim(ASK)

  cli::cli_text('nsim: {.val { dd[1]}}')
  cli::cli_text('nAge: {.val { dd[2]}}')
  cli::cli_text('nBin: {.val { dd[3]}}')
  cli::cli_text('nTS: {.val { dd[4]}}')



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
      if (!is.null(attributes(MeanAtAge)$Years)) {
        cli::cli_text('Year: {.val {attributes(MeanAtAge)$Years[ts[i]]}}')
      } else {
        cli::cli_text('Year: {.val {ts[i]}}')
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

      if (!is.null(attributes(Pars)$Years))
        cli::cli_text('Time Steps: {.val {attributes(Pars)$Years}}')
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

print_slot <- function(object, name) {
  obj <- slot(object, name)
  # chk <- Check(obj)
  # if (chk@empty)
    # return(cli::cli_alert_info('Object is empty'))
  # cli::cli_alert_success('Complete')
}

## Internal Print Functions ----


# PrintPopulating <- function(object, print=TRUE, name=NULL, allup=FALSE) {
#   if (!print)
#     return(NULL)
# 
#   if (!is.null(name)) {
#     cli::cli_progress_message("{cli::symbol$info} Populating {.val {name}}")
#   } else {
#     if (allup)
#       cli::cli_progress_message("{cli::symbol$info}Populating {.val {toupper(class(object))}}")
#     if (!allup)
#       cli::cli_progress_message("{cli::symbol$info} Populating {.val {firstup(class(object))}}")
#   }
# 
# 
# }
# 
# PrintDonePopulating <- function(object, sb, print=TRUE, name=NULL, allup=FALSE) {
#   if (!print)
#     return(NULL)
# 
#   cli::cli_progress_done(id = sb)
# 
#   if (!is.null(name)) {
#     cli::cli_alert_success("Populated {.val {name}}")
#   } else {
#     if (allup)
#       cli::cli_alert_success("Populated {.val {toupper(class(object))}}")
#     if (!allup)
#       cli::cli_alert_success("Populated {.val {firstup(class(object))}}")
#   }
# 
# }



