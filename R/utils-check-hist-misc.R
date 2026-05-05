CheckHistMisc <- function(Hist, Period=c('Historical', 'Projection')) {
  Period <- match.arg(Period)
  
  nSim <- nSim(Hist)
  nStock <- nStock(Hist)
  nFleet <- nFleet(Hist)
  nArea <- nArea(Hist)
  
  HistYears <- Years(Hist, 'H')
  ProjYears <- Years(Hist, 'P')
  
  if (Period=='Historical') {
    Years <- HistYears
  } else {
   Years <-  c(HistYears, ProjYears)
  }
  nyears <- length(Years)
  
  check_length <- function(Misc, expected, name) {
    provided <- length(Misc[[name]])
    if (provided != expected) {
      cli::cli_abort(c("Invalid {.val {name}} in {.val Hist@Misc}",
                     "i"='Expected length:  {.val {expected}}',
                     'i'='Provided length: {.val {provided}}'),
      internal=TRUE)
    }
    invisible(TRUE)
  }
  
  check_rank <- function(array, dd, name) {
    if (length(dim(array) == dd))
      return(invisible(TRUE))
    cli::cli_abort(c("x"="Invalid {.val {name}} in {.val Hist@Misc}",
                     "i"="Should have dimensions: {dd}",
                     "i"="Had dimensions: {length(dim(array))}"),
                     .internal=TRUE)
  }
  
  check_dims <- function(array, expected, name, Years=NULL) {
    dd <- dim(array)
    dnames <- dimnames(array)
    if (is.null(dd) || length(dd)!=length(expected) || is.null(dnames))
      cli::cli_abort("Invalid {.val {name}} in {.val Hist@Misc}", .internal=TRUE)
    
    bad <- which(dd < expected & dd != 1)
    if (length(bad) > 0) {
      cli::cli_abort(c("x"='{.val {name}} has wrong dimensions along axis/axes {.val { paste(bad, collapse = ", ")}}',
                       'i'='Got {.val {paste(bad, collapse = " x ")}}',
                       'i'='Expected {.val {paste(expected, collapse = " x ")}}',
                       .internal=TRUE))
    }
    
    if ("Year" %in% names(dnames)) {
      provided <- dnames[["Year"]]
      ind <- which(!Years %in% provided)
      if (length(ind)) {
        cli::cli_abort(c("x"="Invalid Years in {.val {name}} in {.val Hist@Misc}",
                         "i"="Expected Years: {.val {Years}}",
                         "i"="Provided Years: {.val {provided}}",
                         "i"="Missing Years: {.val {Years[ind]}}"
                         ), .internal=TRUE)
      }
    }
    invisible(TRUE)
  }
  
  # Check Misc list
  Misc <- Hist@Misc
  
  if (!"maxF" %in% names(Misc)) cli::cli_abort("Hist@Misc$maxF is missing", .internal=TRUE)
  maxF <- Misc[["maxF"]]
  if (!is.finite(maxF) || maxF < 0)
    cli::cli_abort("Invalid `maxF` in Hist@Misc", .internal=TRUE)
  
  check_length(Misc, nStock, "SPFrom")
  check_length(Misc, nStock, "PlusGroup")

  check_dims(Misc[["RelSize"]], expected=c(nSim, nArea), name="RelSize")
  check_dims(Misc[["SpawnTimeFrac"]], expected=c(nSim, nStock), name="SpawnTimeFrac")
 
  check_length(Misc, nStock, "LengthList")
  purrr::imap(Misc[["LengthList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears), "LengthList", Years=Years)
  })
  
  check_length(Misc, nStock, "WeightList")
  purrr::imap(Misc[["WeightList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears), "WeightList", Years=Years)
  })         
  
  check_length(Misc, nStock, "NaturalMortalityList")
  purrr::imap(Misc[["NaturalMortalityList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears), "NaturalMortalityList", Years=Years)
  })         
  
  check_length(Misc, nStock, "MaturityList")
  purrr::imap(Misc[["MaturityList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears), "MaturityList", Years=Years)
  })  
 
  check_length(Misc, nStock, "SemelparousList")
  purrr::imap(Misc[["SemelparousList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears), "SemelparousList", Years=Years)
  })  
  
  check_length(Misc, nStock, "FecundityList")
  purrr::imap(Misc[["FecundityList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears), "FecundityList", Years=Years)
  })
   
  check_length(Misc, nStock, "MovementList")
  purrr::imap(Misc[["MovementList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nArea, nArea, nages, nyears), "MovementList", Years=Years)
  })
  
  for (nm in c("SRR_Pars", "RecDevs", "RecLag", "SRR_Model")) {
    if (!nm %in% names(Misc)) cli::cli_abort("Hist@Misc$ {nm} is missing", .internal=TRUE)
  }
  
  check_length(Misc, nStock, "SRR_Pars")
  for (st in seq_len(nStock)) {
    pars_st <- Misc$SRR_Pars[[st]]
    if (length(pars_st) != 1)
      cli::cli_abort("SRR requires exactly 1 parameter array for stock {st}", .internal=TRUE)
    check_rank(pars_st[[1]], 2, paste0("SRR_Pars[[", st, "]][[1]]"))
    check_rank(Misc$RecDevs[[st]], 2, paste0("RecDevs[[", st, "]]"))
    
    if (Misc$RecLag[[st]] < 0)
      cli::cli_abort("RecLag must be non-negative for stock {st}", .internal=TRUE)
    
    model <- Misc$SRR_Model[[st]]
    if (!model %in% 0:2)
      cli::cli_abort("Invalid SRR_Model for stock {st}", .internal=TRUE)
  }
  
  check_length(Misc, nStock, "RecDevs")
  purrr::imap(Misc[["RecDevs"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nyears), "RecDevs", Years=Years)
  })
  
  check_dims(Misc[["RecDist"]], expected=c(nSim,nStock, nyears, nArea), "RecDist", Years=Years)


  check_dims(Misc[["SP0"]], c(nSim, nStock, nyears), "SP0", Years)
  check_dims(Misc[["R0"]], c(nSim, nStock, nyears), "R0", Years)
  
  check_dims(Misc[["Catchability"]], c(nSim, nStock, nyears, nFleet), "Catchability", Years)
  check_dims(Misc[["Closure"]], c(nSim, nStock, nyears, nFleet, nArea), "Closure", Years)
  check_dims(Misc[["Spatial_Targeting"]], c(nSim, nyears, nFleet), "Spatial_Targeting", Years)
  
  check_length(Misc, nStock, "WeightFleetList")
  purrr::imap(Misc[["WeightFleetList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears, nFleet), "WeightFleetList", Years=Years)
  })
  
  check_length(Misc, nStock, "SelAgeList")
  purrr::imap(Misc[["SelAgeList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears, nFleet, nArea), "SelAgeList", Years=Years)
  })
  
  check_length(Misc, nStock, "SelSizeList")
  purrr::imap(Misc[["SelSizeList"]], \(stock, i) {
    purrr::map(stock, \(array) {
      nclass <- dim(array)[2]
      check_dims(array, expected=c(nSim, nclass, nyears, nArea), "SelSizeList", Years=Years)
    })
  })
  
  check_length(Misc, nStock, "RetAgeList")
  purrr::imap(Misc[["RetAgeList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears, nFleet, nArea), "RetAgeList", Years=Years)
  })
  
  check_length(Misc, nStock, "RetSizeList")
  purrr::imap(Misc[["RetSizeList"]], \(stock, i) {
    purrr::map(stock, \(array) {
      nclass <- dim(array)[2]
      check_dims(array, expected=c(nSim, nclass, nyears, nArea), "RetSizeList", Years=Years)
    })
  })
  

  check_length(Misc, nStock, "DiscMortList")
  purrr::imap(Misc[["DiscMortList"]], \(array, i) {
    nages <- nAge(Hist@OM@Stock[[i]])
    check_dims(array, expected=c(nSim, nages, nyears, nFleet, nArea), "DiscMortList", Years=Years)
  })
  
  check_length(Misc, nStock, "DiscMortSizeList")
  purrr::imap(Misc[["DiscMortSizeList"]], \(stock, i) {
    purrr::map(stock, \(array) {
      nclass <- dim(array)[2]
      check_dims(array, expected=c(nSim, nclass, nyears, nArea), "DiscMortSizeList", Years=Years)
    })
  })
  
  check_rank(Misc$StockTargeting, 4, 'StockTargeting')
  check_dims(Misc$StockTargeting,
             expected=c(nSim, nStock, nFleet, nyears),
             name = StockTargeting, 
             Years=Years) 

  invisible(TRUE)
}


