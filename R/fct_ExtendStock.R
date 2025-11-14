
ExtendStock <- function(Stock, nSim, Years, silent=FALSE, id=NULL) {
  if (!silent)
    cli::cli_progress_update(id=id)
  
  nAges <- length(Stock@Ages@Classes)
  
  Stock@Length <- ExtendStockObject(Stock@Length, nSim, nAges, Years)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Stock@Weight <- ExtendStockObject(Stock@Weight, nSim, nAges, Years)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@NaturalMortality <- ExtendStockObject(Stock@NaturalMortality, nSim, nAges, Years)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Maturity <- ExtendStockObject(Stock@Maturity, nSim, nAges, Years)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@Fecundity <- ExtendStockObject(Stock@Fecundity, nSim, nAges, Years)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  Stock@SRR <- ExtendStockObject(Stock@SRR, nSim, nAges, Years)
  
  Stock@SRR@R0 <- ArrayExpand(Stock@SRR@R0, nSim, nAges=NULL, Years)
  Stock@SRR@SD <- ArrayExpand(Stock@SRR@SD, nSim, nAges=NULL, Years)
  Stock@SRR@AC <- ArrayExpand(Stock@SRR@AC, nSim, nAges=NULL, Years)
  
  Stock@SRR@RecDevHist <- Stock@SRR@RecDevHist |> ExtendSims(nSim)
  Stock@SRR@RecDevProj <- Stock@SRR@RecDevProj |> ExtendSims(nSim)
  
  if (is.null(Stock@SRR@SPFrom)) {
    Stock@SRR@SPFrom <- Stock@Name
  }
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Stock@Spatial@UnfishedDist <- ArrayExpand(Stock@Spatial@UnfishedDist, nSim, nAges, Years)
  Stock@Spatial@ProbStaying <- ArrayExpand(Stock@Spatial@ProbStaying, nSim, nAges, Years)
  Stock@Spatial@RelativeSize <- ArrayExpand(Stock@Spatial@RelativeSize, nSim, nAges, Years)
  
  if (is.list(Stock@Spatial@Movement)) {
    MoveYears <- names(Stock@Spatial@Movement)
    AddYears <- Years[!Years %in% MoveYears]
    if (length(AddYears)) {
      AddMovement <- MakeNamedList(AddYears, Stock@Spatial@Movement[[length(Stock@Spatial@Movement)]])
      Stock@Spatial@Movement <- c(Stock@Spatial@Movement, AddMovement)
    }
  } else {
    Stock@Spatial@Movement <- ArrayExpand(Stock@Spatial@Movement, nSim, nAges, Years)  
  }
  
  Stock@Spatial@FracOther <- ArrayExpand(Stock@Spatial@FracOther, nSim, nAges, Years)
  Stock@Spatial@Arrangement <- ArrayExpand(Stock@Spatial@Arrangement, nSim, nAges, Years)
  
  if (!silent)
    cli::cli_progress_update(id=id)
  
  Stock@Depletion@Initial <- ExtendSims(Stock@Depletion@Initial, nSim)
  Stock@Depletion@Final <- ExtendSims(Stock@Depletion@Final, nSim)
  
  Stock
}




