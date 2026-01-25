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

## ---- Ages ----

setMethod("show", "ages", function(object) {
  
  cli::cli_par()
  cli::cli_h2("An {.help MSEtool::Ages} Object")
  
  cli::cli_ul(c(
    "Minimum age: {.val {object@MinAge}}",
    "Maximum age: {.val {object@MaxAge}}",
    "Units: {.val {object@Units}}",
    if (isTRUE(object@PlusGroup)) {
      "Plus group: {.emph enabled} (maximum age is a plus group)"
    } else {
      "Plus group: {.emph disabled}"
    },
    {
      AgeClasses <- CalcAgeClasses(object)
      if (!is.null(AgeClasses)) {
        if (isTRUE(object@PlusGroup)) {
          AgeClasses[length(AgeClasses)] <- paste0(AgeClasses[length(AgeClasses)], "+")
        } 
      }

      "Age classes: {.val {AgeClasses}}"
    }
  ))
  
  cli::cli_end()
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




## ---- Spatial ----
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

show_slot <- function(value) {
  # show arrays, matrices, numeric, etc 
}


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
