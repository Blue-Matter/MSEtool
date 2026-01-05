library(MSEtool)

la()

OM <- ImportBAM('Gag Grouper')

# pop dynamics should be the same for both herm and non-herm models ... 

# Make two stocks 
Female <- Stock(OM)[[1]]
Name(Female) <- 'Female'

Male <- Stock(OM)[[1]]
Male |> SRR() |> SPFrom() <- 1 # spawning production from female
Name(Male) <- 'Male'

# Add the stocks
Stock(OM) <- list(Female, Male)


# Duplicate the fleets
Fleet(OM) <- list(Fleet(OM)[[1]], Fleet(OM)[[1]]) 

# Duplicate the fleets
Fleet(OM) <- list(Fleet(OM)[[1]], Fleet(OM)[[1]]) 


AgeClasses <- Male |> Classes()

Herm(OM) <- list('H_2_1') # Sim, Age, Year 

# TODO -
# herm can sometimes depend on abundance etc ...
# 



  
  
OM <- Populate(OM)




Fleet(GagGrouper_BAM) |> length()






PopulateOM(GagGrouper)

OM <- GagGrouper

herring <- Convert(Herring)


OM <- OM('Silly',
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


Convert(Herring)





