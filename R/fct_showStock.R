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


## ---- Length ----

setMethod("show", "length", function(object) {
  
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Length} Object")
  
  cli::cli_ul(c(
    
    # Growth model
    "Model: {.val {object@Model}}",
    
    # Units
    "Units: {.val {object@Units}}",
    
    # Parameters
    paste0(
      "Parameters: ",
      if (length(object@Pars) == 0) {
        "{.emph not specified}"
      } else {
        paste0(
          "{.val ", length(object@Pars), "} parameter",
          if (length(object@Pars) > 1) "s"
        )
      }
    ),
    
    # Mean at age
    paste0(
      "Mean length-at-age: ",
      .format_array(object@MeanAtAge, "MeanAtAge")
    ),
    
    # CV at age
    paste0(
      "CV at age: ",
      .format_array(object@CVatAge, "CVatAge")
    ),
    
    # Distribution
    "Distribution: {.val {object@Dist}}",
    
    # Truncation
    "Truncation (SD): {.val {object@TruncSD}}",
    
    # Timing
    "Timing: {.val {object@Timing}}",
    
    # Random effects
    paste0(
      "Random effects: ",
      if (is.null(object@Random) || length(object@Random) == 0) {
        "{.emph none}"
      } else {
        "{.val specified}"
      }
    ),
    
    # Age–length key
    paste0(
      "Age–length key: ",
      if (is.null(object@ASK) || length(object@ASK) == 0) {
        "{.emph not specified}"
      } else {
        .format_array(object@ASK, "ASK")
      }
    ),
    
    # Length classes
    paste0(
      "Length classes: ",
      if (is.null(object@Classes) || length(object@Classes) == 0) {
        "{.emph not specified}"
      } else {
        paste0(
          "{.val ", length(object@Classes), "} bins: ",
          "{.val { .format_vec(object@Classes) }}"
        )
      }
    )
    
  ))
  
  cli::cli_end()
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
