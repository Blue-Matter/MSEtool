# 

library(MSEtool)

if (!packageVersion('MSEtool') >= '4.0.0')
  stop('Needs MSEtool v4+')

MOM <- readRDS('C:/Users/Admin/Downloads/MOM.rds')
MOM <- readRDS('C:/Users/Adrian/Downloads/MOM.rds')

# Convert to new structure
OM <- Convert(MOM, Seasons=4) 

# Note: not including real fishery data at this point !!!

# ----- Weird Issue - from RCM presumably: ------
# Rec devs go to 0 in projection period:
data.frame(OM=c(rev(OM@Stock$Dolphinfish@SRR@RecDevInit[1,]),
                OM@Stock$Dolphinfish@SRR@RecDevHist[1,],
                OM@Stock$Dolphinfish@SRR@RecDevProj[1,]),
           MOM=MOM@cpars[[1]][[1]]$Perr_y[1,]) |>
  round(3)

MOM@cpars[[1]][[1]]$Perr_y[1,] |> plot()


# -----------------------------------------------

# Simulate Historical
Hist <- Simulate(OM) 


multiHist <- Simulate(MOM)


# Compare Historical N 
sim <- sample(1:OM@nSim, 1)
OM_N <- Number(Hist) |> dplyr::filter(Sim==sim) 
MOM_N <- apply(multiHist$`Stock 1`$`Fleet 1`@TSdata$Number[sim,,], 1, sum)

plot(OM_N$Year, OM_N$Value, type='l', ylim=c(0, max(OM_N$Value)))
lines(OM_N$Year, MOM_N, col='blue')

# Compare Historical Catch
sim <- sample(1:OM@nSim, 1)

OM_Landings <- Landings(Hist, byFleet=TRUE) |> dplyr::filter(Sim==sim) 

MOM_Landings <- matrix(0, 148, nFleet(OM))
for (fl in 1:nFleet(OM)) {
  MOM_Landings[,fl] <- rowSums(multiHist[[1]][[fl]]@TSdata$Landings[sim,,])  
}

par(mfrow=c(2,4))
fleets <- OM_Landings$Fleet |> unique()
for (fl in 1:nFleet(OM)) {
  df <- OM_Landings |> dplyr::filter(Fleet==fleets[fl])
  plot(df$Year, df$Value, type='l', ylim=c(0, max(df$Value)))
  lines(df$Year, MOM_Landings[,fl], col='blue', lty=2)
  title(fleets[fl])
}


# Spatial Stuff 
Spatial <- OM@Stock$Dolphinfish@Spatial

# Slots to Populate:

# Unfished Distribution 
# array with dimensions nsim, narea, narea, nYears
# dimension `sim` and `year` can be length 1 if unfished dist constant over
# simulations and/or years 
Spatial@UnfishedDist 

# array with dimensions nsim, narea, nAge, nYears
# dimension `sim`, `age` and `year` can be length 1 if unfished dist constant over
# simulations, age, and/or years 
Spatial@ProbStaying 

Spatial@RelativeSize # matrix nsim by nArea (again sim dim can be length 1 if constant)

Spatial@Movement |> dimnames()


Spatial@FracOther 

OM@Stock$Dolphinfish@Spatial <- Spatial # reassign once populated 



# Fleet Effort Distribution - repeat for each fleet
# fishing effort
OM@Fleet$Dolphinfish$`Dolphinfish Fleet 1`@Effort 



# fishing effort by sim, time, and area 
OM@Fleet$Dolphinfish$`Dolphinfish Fleet 1`@Distribution 



  

