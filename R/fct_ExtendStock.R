ExtendStock <- function(Stock, nSim, Years, silent = FALSE, id = NULL) {
  if (!silent) {
    cli::cli_progress_update(id = id)
  }

  AgeClasses <- Stock@Ages@Classes

  Stock@Length <- ExtendStockObject(Stock@Length, nSim, AgeClasses, Years)

  if (!silent) {
    cli::cli_progress_update(id = id)
  }

  Stock@Weight <- ExtendStockObject(Stock@Weight, nSim, AgeClasses, Years)

  if (!silent) {
    cli::cli_progress_update(id = id)
  }
  Stock@NaturalMortality <- ExtendStockObject(Stock@NaturalMortality, nSim, AgeClasses, Years)

  if (!silent) {
    cli::cli_progress_update(id = id)
  }

  Stock@Maturity <- ExtendStockObject(Stock@Maturity, nSim, AgeClasses, Years)


  if (!silent) {
    cli::cli_progress_update(id = id)
  }
  Stock@Fecundity <- ExtendStockObject(Stock@Fecundity, nSim, AgeClasses, Years)

  if (!silent) {
    cli::cli_progress_update(id = id)
  }
  Stock@SRR <- ExtendStockObject(Stock@SRR, nSim, AgeClasses, Years)


  Stock@SRR@R0 <- Extend(Stock@SRR@R0, nSim, AgeClasses, Years)
  Stock@SRR@SD <- Extend(Stock@SRR@SD, nSim, AgeClasses, Years)
  Stock@SRR@AC <- Extend(Stock@SRR@AC, nSim, AgeClasses, Years)

  Stock@SRR@RecDevHist <- Stock@SRR@RecDevHist |> ExtendSims(nSim)
  Stock@SRR@RecDevProj <- Stock@SRR@RecDevProj |> ExtendSims(nSim)

  if (is.null(Stock@SRR@SPFrom)) {
    Stock@SRR@SPFrom <- Stock@Name
  }

  if (!silent) {
    cli::cli_progress_update(id = id)
  }

  Stock@Spatial@UnfishedDist <- Extend(Stock@Spatial@UnfishedDist, nSim, AgeClasses, Years)
  Stock@Spatial@ProbStaying <- Extend(Stock@Spatial@ProbStaying, nSim, AgeClasses, Years)
  Stock@Spatial@RelativeSize <- Extend(Stock@Spatial@RelativeSize, nSim, AgeClasses)

  if (is.list(Stock@Spatial@Movement)) {
    MoveYears <- names(Stock@Spatial@Movement)
    AddYears <- Years[!Years %in% MoveYears]
    if (length(AddYears)) {
      AddMovement <- MakeNamedList(AddYears, Stock@Spatial@Movement[[length(Stock@Spatial@Movement)]])
      Stock@Spatial@Movement <- c(Stock@Spatial@Movement, AddMovement)
    }
  } else {
    Stock@Spatial@Movement <- Extend(Stock@Spatial@Movement, nSim, AgeClasses, Years)
  }

  Stock@Spatial@FracOther <- Extend(Stock@Spatial@FracOther, nSim, AgeClasses, Years)
  Stock@Spatial@Arrangement <- Extend(Stock@Spatial@Arrangement, nSim, AgeClasses, Years)

  if (!silent) {
    cli::cli_progress_update(id = id)
  }

  Stock@Depletion@Initial <- ExtendSims(Stock@Depletion@Initial, nSim)
  Stock@Depletion@Final <- ExtendSims(Stock@Depletion@Final, nSim)

  Stock
}


ExtendStockObject <- function(object, nSim, AgeClasses, Years) {
  nms <- slotNames(object)

  if ("Pars" %in% nms) {
    if (!all(unlist(lapply(object@Pars, is.na)))) {
      object@Pars <- purrr::map(object@Pars, \(Par) 
                                Extend(Par, nSim, AgeClasses=AgeClasses, Years)
      )
    }
  }

  if ("MeanAtAge" %in% nms) {
    object@MeanAtAge <- Extend(object@MeanAtAge, nSim, AgeClasses, Years)
  }

  if ("CVatAge" %in% nms) {
    object@CVatAge <- Extend(object@CVatAge, nSim, AgeClasses, Years)
  }

  if ("MeanAtLength" %in% nms) {
    object@MeanAtLength <- Extend(object@MeanAtLength, nSim, NULL, Years = NULL)
  }

  if ("MeanAtWeight" %in% nms) {
    object@MeanAtWeight <- Extend(object@MeanAtWeight, nSim, NULL, Years)
  }

  if ("Semelparous" %in% nms) {
    object@Semelparous <- Extend(object@Semelparous, nSim, AgeClasses, Years)
  }

  if ("Random" %in% nms) {
    object@Random <- Extend(object@Random, nSim, AgeClasses, Years)
  }

  # if ("ASK" %in% nms) # too large in some cases and not needed
  #   object@ASK <- Extend(object@ASK, nSim, nAges, Years)

  object
}
