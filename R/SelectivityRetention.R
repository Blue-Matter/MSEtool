


#' Calculate Selectivity-at-Age from Total Fishing-Mortality-atAge
#'
#' @param FishingMortality A [FishingMortality()] object with the `DeadAtAge` populated. The `RetainAtAge` slot is only required if it is
#' different than `DeadAtAge`; i.e., there are dead discards.
#' @param DiscardMortality A [DiscardMortality()] object with `MeanAtAge` populated. Only required if `RetainAtAge` is populated and different
#' than `DeadAtAge`
#' @param Ages An [Ages()] object.
#' @param TimeSteps A numeric vector indicating the time steps#'
#' @export
FishingMortality2Selectivity <- function(FishingMortality, DiscardMortality, Ages=NULL,
                                         TimeSteps=NULL, Length=NULL) {

  # Fishing 'mortality' for all fish that interact with gear, including
  # those discarded alive
  FInteract <- CalculateFInteract(FishingMortality,
                                  DiscardMortality,
                                  Ages,
                                  TimeSteps,
                                  Length)

  nAge <- dim(FishingMortality@DeadAtAge)[2]
  maxValues <- apply(FInteract, c(1,3), max)
  maxValues <- replicate(nAge, maxValues) |> aperm(c(1,3,2))
  out <- FInteract/maxValues
  out[!is.finite(out)] <- 0
  out
}

FishingMortality2Retention <- function(FishingMortality,
                                       DiscardMortality=NULL,
                                       Ages=NULL,
                                       TimeSteps=NULL,
                                       Length=NULL,
                                       seed=NULL,
                                       silent=FALSE) {

  FRetainAtAge <- AddSimDimension(FishingMortality@RetainAtAge, TimeSteps=TimeSteps)
  FInteract <- CalculateFInteract(FishingMortality,
                                  DiscardMortality,
                                  Ages,
                                  TimeSteps,
                                  Length)
  Retention <- ArrayDivide(FRetainAtAge, FInteract)
  Retention[!is.finite(Retention)] <- 0
  Retention
}



CalculateFInteract <- function(FishingMortality, DiscardMortality,
                               Ages=NULL,
                               TimeSteps=NULL, Length=NULL) {

  DeadAtAge <- AddSimDimension(FishingMortality@DeadAtAge, TimeSteps=TimeSteps)
  nAge <- dim(DeadAtAge)[2]
  if (is.null(FishingMortality@RetainAtAge)) {
    return(DeadAtAge)
  }

  # account for discard mortality
  # chk <- CheckRequiredObject(DiscardMortality, 'discardmortality', 'DiscardMortality')
  # if (!chk@populated)
  #   DiscardMortality <- PopulateDiscardMortality(DiscardMortality,
  #                                Ages,
  #                                Length,
  #                                nsim,
  #                                TimeSteps,
  #                                seed=seed,
  #                                silent=silent)

  RetainAtAge <- AddSimDimension(FishingMortality@RetainAtAge, TimeSteps=TimeSteps)
  DiscardAtAge <- ArraySubtract(DeadAtAge, RetainAtAge)

  outdims <- rbind(dim(DeadAtAge), dim(RetainAtAge)) |> apply(2, max)

  DiscardMortalityAtAge <- array(NA, dim=outdims)
  dd <- dim(DiscardMortality@MeanAtAge)

  if (is.null(attributes(DiscardMortality)$TimeSteps)) {
    DiscardMortalityAtAge <- AddSimDimension(DiscardMortality@MeanAtAge, TimeSteps = TimeSteps)
  } else {
    discTimeSteps <- attributes(DiscardMortality)$TimeSteps
    if (is.null(TimeSteps))
      cli::cli_abort("`TimeSteps` must be a numeric vector")
    ind <- which(TimeSteps %in% discTimeSteps)
    if (sum(ind)<1)
      cli::cli_abort("`TimeSteps` must include values in `attributes(DiscardMortality)$TimeSteps`")
    if (dd[2]==1) {
      DiscardMortalityAtAge[,,ind] <- t(replicate(nAge,DiscardMortality@MeanAtAge[,1,]))
    } else {
      DiscardMortalityAtAge[,,ind] <- DiscardMortality@MeanAtAge
    }
    for (i in seq_along(ind)) {
      if (ind[i] == length(TimeSteps))
        break()
      st <- ind[i]
      end <- ind[i+1]-1
      end[is.na(end)] <- outdims[3]
      DiscardMortalityAtAge[,,st:end] <- DiscardMortalityAtAge[,,st]
    }
  }
  ArrayAdd(RetainAtAge,ArrayDivide(DiscardAtAge,DiscardMortalityAtAge))

}
