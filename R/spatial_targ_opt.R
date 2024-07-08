solve_spat_targ <- function(par, pop, qsx, qfracx, Ecur, n_age, nareas, Fdist, 
                            Vcur, N_area, M_age, VBcur_a, f_zero_fleet, Asizex) {
  Spat_targ <- par
  if (length(f_zero_fleet)>0) {
    dummy <- rep(0, length(f_zero_fleet))
    Spat_targ <- rep(NA, length(c(par, f_zero_fleet)))
    Spat_targ[f_zero_fleet] <- 0
    Spat_targ[is.na(Spat_targ)] <- par
  }

  # overall F
  F_fleet <- qsx[pop] * qfracx[pop,] * Ecur[pop,]
  nf <- length(F_fleet)
  F_total <- sum(F_fleet)
  
  ind <- TEG(c(1, nf, n_age, nareas))
  ind[,1] <- pop
  F_area <- Z_area <- C_area <- array(NA, dim=c(nf, n_age, nareas))
  Fdist <- array(NA, dim=c(nf, n_age, nareas))
  
  Fdist[ind[,c(2:4)]] <- VBcur_a[ind[,c(1,2,4)]]^Spat_targ[ind[,2]]
  Fdist[!is.finite(Fdist)] <- 0
  
  for (f in 1:nf) {
    Fdist[f,,] <- Fdist[f,,]/rowSums(Fdist[f,,])
  }
  
  Fdist[is.na(Fdist)] <- 0 # This is an NA catch for hermaphroditism
  F_area[ind[,c(2:4)]] <-  qsx[ind[,1]]  * qfracx[ind[,1:2]] * Ecur[ind[,1:2]] * 
    Fdist[ind[,c(2:4)]] * Vcur[ind[,1:3]] / Asizex[ind[, c(1, 4)]]
  
  M_area <- replicate(nareas, M_age) 
  Z_area[ind[,c(2:4)]] <- F_area[ind[,c(2:4)]] + M_area[ind[,3:4]]
  
  C_area[ind[,c(2:4)]] <- F_area[ind[,c(2:4)]]/Z_area[ind[,c(2:4)]] * 
    (1-exp(-Z_area[ind[,c(2:4)]])) * N_area[ind[,c(3:4)]]
  
  C_area[!is.finite(C_area)] <- 0 
  
  # Catch for total F
  F_age <- replicate(n_age,F_fleet) *  Vcur[pop,,]
  if (!is.null(dim(F_age))) {
    Z_age <- apply(F_age, 2, sum) + M_age
  } else {
    Z_age <- F_age + M_age
  }
 
  Z_age <- t(replicate(nf, Z_age))
  
  N_total <- apply(N_area, 1, sum)
  N_total <- replicate(nf, N_total) |> t()
  C_total <- F_age/Z_age * (1-exp(-Z_age)) * N_total 
  C_total[!is.finite(C_total)] <- 0
  
  # par(mfrow=c(2,2))
  # c_pred <- apply(C_area,1:2, sum)
  # for (fl in 1:nf) {
  #   plot(C_total[fl,], type='l')
  #   lines(c_pred[fl,], col='blue')
  # }
  # 
  sum((C_total - apply(C_area,1:2, sum))^2)
}
