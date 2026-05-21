VBiomass <- function(Hist, Year, type=c('Removals', 'Landings')) {
  CheckClass(Hist, c('hist', 'mse'), 'Hist')
  
  
  

  
}

CalcVBiomass <- function(Hist, Year, type=c('Removals', 'Landings')) {
  
  type <- match.arg(type)
  
  fleet_names <- FleetNames(Hist)
  n_fleet <-length(fleet_names)
  n_sim <- nSim(Hist)
  n_area <- nArea(Hist)
  
  Complexes <- Hist@OM@Complexes
  Allocation <- Hist@OM@Allocation
  n_complex <- length(Complexes)

  NumberAtAge <- purrr::map(Hist@Number, \(stock) 
                            Subset(stock, Years=Year) |>
                              DropDimension('Year'))
  
  WeightAtAge <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@WeightFleet |> Subset(Years=Year) |> DropDimension('Year')
    }) |> List2Array()
  })
  
  Selectivity <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@Selectivity@MeanAtAge |> Subset(Years=Year) |> DropDimension('Year')
    }) |> List2Array()
  })
  
  Retention <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@Retention@MeanAtAge |> Subset(Years=Year) |> DropDimension('Year')
    }) |> List2Array()
  })
  
  DiscardMortality <- purrr::map(Hist@OM@Fleet, \(FleetList) {
    purrr::map(FleetList, \(Fleet) {
      Fleet@DiscardMortality@MeanAtAge |> Subset(Years=Year) |> DropDimension('Year')
    }) |> List2Array()
  })
  
  out <- array(NA, dim=c(n_sim, n_complex, n_fleet),
               dimnames = list(
                 Sim=seq_len(n_sim),
                 Complex=names(Complexes),
                 Fleet=fleet_names
               ))
  
  eff_sel_fn <- if (type == 'Landings') {
    function(sel, ret, dm) ArrayMultiply(sel, ret)
  } else {
    # removals = retained + dead discards
    function(sel, ret, dm) ArrayMultiply(sel, ArraySum(ret, ArrayMultiply(1- ret, dm)))
  }
  
  
  for (i in seq_len(n_complex)) {
    stocks <- Complexes[[i]]
    n_stock <- length(stocks)
    alloc <- Allocation[[i]]
    
    stock_vb <- array(NA, dim=c(n_sim, n_stock, n_fleet),
                      dimnames = list(
                        Sim=seq_len(n_sim),
                        Stock=stocks,
                        Fleet=fleet_names
                      ))
    
    for (st in stocks) {
      n <- NumberAtAge[[st]] |> AddDimension('Fleet') |> ExtendFleets(Fleets=fleet_names)
      w <-  WeightAtAge[[st]] |> AddDimension('Area', pos=3) |> ExtendAreas(Areas=seq_len(n_area)) 
      b <- ArrayMultiply(n, w) 
      
      for (fl in seq_len(n_fleet)) {
        
        sel <- Selectivity[[st]][,,,fl,drop=FALSE] |> DropDimension('Fleet')
        ret <- Retention[[st]][,,,fl,drop=FALSE] |> DropDimension('Fleet')
        dm  <- DiscardMortality[[st]][,,,fl,drop=FALSE] |> DropDimension('Fleet') 
        eff_sel  <- eff_sel_fn(sel, ret, dm)
        b_fleet <- b[,,,fl,drop=FALSE] |> DropDimension('Fleet') 
        alloc_fleet <- alloc[,fl] 
        vb <- SumOverAge(ArrayMultiply(b_fleet, eff_sel)) |> SumOverArea()
        
        stock_vb[,st, fl] <- vb * alloc_fleet
      }
    }
    out[,i,] <- SumOverStock(stock_vb)
  }
  out
}
