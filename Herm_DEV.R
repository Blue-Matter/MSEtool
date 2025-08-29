# TODO this should be done in Populate

OM@SexPars$Herm$H_2_1 <- c(0,0,0,0,0,0,0,0,0.05,0.1,0.2,0.35,0.65,0.8,0.9,1,1,1,1)
OM@SexPars$Herm$H_3_11<- c(0,0,0,0,0,0,0,0,0.05,0.1,0.2,0.35,0.65,0.8,0.9,1,1,1,1)


StructureHerm <- function(OM) {
  if (!(length(OM@SexPars)))
    OM@SexPars <- list()
  
  if (!(length(OM@SexPars$Herm))) {
    OM@SexPars$HermFrac <- vector('list', nStock(OM))
    for (st in 1:nStock(OM)) {
      nage <- Stock(OM, st) |> nAge()
      OM@SexPars$HermFrac[[st]] <- array(1, dim=c(1, nage, 1))
    }
    return(OM)
  }
}



nHerm <- length(OM@SexPars$Herm)

Herm <- vector('list', nHerm)
HermFrac <- vector('list', nStock(OM))



nStock(OM)


# add dimensions 

# expand for all stocks

nStock(OM)

OM@SexPars$Herm$H_1_2 |> dim()


OM@SexPars$Herm





OM@SexPars$Herm <- Herm
OM@SexPars$HermFrac <- HermFrac
OM


Initial()




control$HermEq


OM@SexPars$Herm
