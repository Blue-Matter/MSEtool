devtools::load_all()
library(MSEtool)


OM <- OM('BFT and Herring MICE Model',
         nSim=30,
         pYear=50)

# Sequential Hermaphroditism - Female to Male
Stock(OM) <- list(Herr_M=OM2stock(Herring),
                  Herr_F=OM2stock(Herring))

Fleet(OM) <- list(list(OM2fleet(Generic_FlatE)),
                  list(OM2fleet(Generic_FlatE)))

#To specify protogyny (Female - Male) where stock 1 is female and stock 2 is 
#  male, you include a vector H_2_1 that is the fraction male (Stock 2) at age:

# fraction in stock 2 at age
nAge(Stock(OM,1))
OM@SexPars$Herm$H_2_1 <- c(0,0,0,0,0,0,0,0,0.05,0.1,0.2,0.35,0.65,0.8,0.9,1,1,1,1)
OM@SexPars$Herm$H_3_11<- c(0,0,0,0,0,0,0,0,0.05,0.1,0.2,0.35,0.65,0.8,0.9,1,1,1,1)

lapply(OM@SexPars$Herm, length)






