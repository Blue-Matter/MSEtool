
GetObject <- function(Stock, slots) {
  object <- Stock
  nlayer <- length(slots)
  for (l in 1:nlayer) {
    object <- slot(object, slots[l])
  }
  object
}


ListSimStock <- function(Stock, slots, exp_dim=2, R6=TRUE) {
  nstock <- length(Stock)
  if (methods::is(Stock, 'stock'))
    Stock <- list(Stock)
  
  nsim <- Stock[[1]]@nSim
  out <- vector('list', nsim)
  for (x in 1:nsim) {
    out[[x]] <- vector('list', nstock)
    for (p in 1:nstock) {
      object <- GetObject(Stock[[p]], slots)
      dd <- dim(object)
      if (all(dd==0)) next()
      d <- x
      if (x>dd[1]) d <- 1
      if (exp_dim==1) {
        val <- abind::adrop(object[d, ,drop=FALSE],1)
        val <- array(val)
      }
      if (exp_dim==2) {
        val <- abind::adrop(object[d,,, drop=FALSE],1)
      }
      if (exp_dim==3) {
        val <- abind::adrop(object[d,,,, drop=FALSE],1)
      }
      if (exp_dim==4) {
        val <- abind::adrop(object[d,,,,, drop=FALSE],1)
      }
      if (R6) {
        out[[x]][[p]] <- R6array$new(val)
      } else {
        out[[x]][[p]] <- val
      }
      
    }
  }
  out
}

ListStock <- function(Stock, slots, R6=FALSE, null_val=0) {
  nstock <- length(Stock)
  if (methods::is(Stock, 'stock'))
    Stock <- list(Stock)
  
  out <- vector('list', nstock)
  for (p in 1:nstock) {
    nlayer <- length(slots)
    object <- Stock[[p]]
    for (l in 1:nlayer) {
      object <- slot(object, slots[l])
    }
    if (R6) {
      out[[p]] <- R6array$new(object)
    } else {
      if (is.null(object)) object <- null_val
      out[[p]] <- object
    }
  }
  out
}


SimStockList <- function(OM, 
                         dims=c('nAge', 'nTS', 'nArea'),
                         names=c('Sim', 'Time Step', 'Area')) {
  out <- vector('list', OM@nSim)
  if (methods::is(OM@Stock, 'stock'))
    OM@Stock <- list(OM@Stock)
  
  nstock <- length(OM@Stock)
  for (x in 1:OM@nSim) {
    out[[x]] <- vector('list', nstock)
    for (p in 1:nstock) {
      ldims <- rep(NA, length(dims))
      for (i in 1:length(dims)) {
        ldims[i] <- get(dims[i])(OM@Stock[[p]])
      }
      out[[x]][[p]] <- R6array$new(array(NA, dim=ldims)) 
      out[[x]][[p]]$value <-  AddDimNames(out[[x]][[p]]$value, names=names)
    }
  }
  out
}


getR6 <- function(val, dev=FALSE) {
  if (dev) {
    val <- val[[1]][[1]]
  }
  if (inherits(val, 'R6'))
    return(val$value)
  val
}

CalcSurvival_Stock <- function(M_at_Age, F_at_Age=NULL, PlusGroup=1, SpawnTimeFrac=NULL) {
  M_at_Age <- getR6(M_at_Age)
  PlusGroup <- getR6(PlusGroup)
  SpawnTimeFrac <- getR6(SpawnTimeFrac)
  
  Z_at_Age <- M_at_Age
  if (!is.null(F_at_Age))
    Z_at_Age <- M_at_Age + F_at_Age
  
  n_age <- nrow(M_at_Age)
  n_TS <- ncol(M_at_Age)
  surv <- matrix(1, n_age, n_TS)
  if (is.null(SpawnTimeFrac)) {
    SpawnTimeFrac <- 0
  }
  
  surv[1,] <- 1 * exp(-M_at_Age[1,]*SpawnTimeFrac)
  for (a in 2:n_age) {
    surv[a,] <- surv[a-1,]*exp(-(Z_at_Age[a-1,]*(1-SpawnTimeFrac)+Z_at_Age[a,]*SpawnTimeFrac))
  }
  if (PlusGroup)
    surv[n_age,] <- surv[n_age,]/(1-exp(-Z_at_Age[n_age,]))
  surv
}


CalcSurvival_Sim <- function(M_at_Age, PlusGroup, SpawnTimeFrac) {
  if (is.null(SpawnTimeFrac))
    SpawnTimeFrac <- vector('list', length(M_at_Age))
  mapply(CalcSurvival_Stock, M_at_Age, PlusGroup=PlusGroup, SpawnTimeFrac=SpawnTimeFrac,
         SIMPLIFY = FALSE)
}

CalcUnfishedSurvival <- function(M_at_Age, PlusGroup, SpawnTimeFrac=NULL) {
  mapply(CalcSurvival_Sim, M_at_Age,
         MoreArgs = list(PlusGroup=PlusGroup, SpawnTimeFrac=SpawnTimeFrac),
         SIMPLIFY = FALSE)
}