# ---- Stock ----

setMethod('show', 'stock', function(object) {
  
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Stock} Object")
  
  
  
  cli::cli_h3('{.code Name}')
  cli::cli_text('{.val { object@Name}}')
  
  cli::cli_h3('{.code CommonName}')
  cli::cli_text('{.val { object@CommonName}}')
  
  cli::cli_h3('{.code Species}')
  cli::cli_text('{.val { object@Species}}')
  
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
  
  for (sl in slots) {
    cli::cli_h3('{.code {sl}}')
    # print_slot(object, sl)
  }
  
  cli::cli_end()
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
  object@Classes <- AgeClasses
  .show_slot(object, 'Classes')
})


setMethod("show", "length", function(object) {
  cli::cli_h2("A {.help MSEtool::Length} Object")
  .show_object(object)
})

setMethod("show", "weight", function(object) {
  cli::cli_h2("A {.help MSEtool::Weight} Object")
  .show_object(object)
})

setMethod("show", "naturalmortality", function(object) {
  cli::cli_h2("A {.help MSEtool::NaturalMortality} Object")
  .show_object(object)
})


setMethod("show", "maturity", function(object) {
  cli::cli_h2("A {.help MSEtool::Maturity} Object")
  .show_object(object)
})
  
setMethod("show", "fecundity", function(object) {
  cli::cli_h2("A {.help MSEtool::Fecundity} Object")
  .show_object(object)
})

setMethod("show", "srr", function(object) {
  cli::cli_h2("A {.help MSEtool::SRR} Object")
  
  .show_slot(object, 'R0')
  .show_slot(object, 'SD')
  .show_slot(object, 'AC')
  .show_slot(object, 'SPFrom')
  
  .show_array(object@RecDevInit, 'RecDevInit')
  .show_array(object@RecDevHist, 'RecDevHist')
  .show_array(object@RecDevProj, 'RecDevProj')
  
  .show_slot(object, 'SpawnTimeFrac')
  
  .show_object(object)
})



#' @rdname show
setMethod('show', 'spatial', function(object) {
  
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Spatial} Object")
  slots <- slotNames(object)
  for (sl in slots) {
    cli::cli_h3('{.code {sl}}')
    cli::cli_text('{.val { slot(object,sl)}}')
  }
  cli::cli_end()
})



## ---- Depletion ----

setMethod("show", "depletion", function(object) {
  
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Depletion} Object")
  
  cli::cli_ul(c(
    "Reference biomass: {.val {object@Reference}}",
    if (length(object@Initial) == 0) {
      "Initial depletion: {.emph not specified} (assumed dynamic unfished)"
    } else {
      "Initial depletion: {.val { .format_sim_values(object@Initial) }}"
    },
    if (length(object@Final) == 0) {
      "Final depletion: {.emph not specified} (determined by historical dynamics)"
    } else {
      "Final depletion: {.val { .format_sim_values(object@Final) }}"
    }
    
  ))
  
  cli::cli_end()
})
