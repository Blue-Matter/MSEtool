library(MSEtool)

la()

testOM@nsim <- 50



OM <- ConvertOM(testOM) 


Hist <- Simulate(OM)

cbind(Hist@FDeadArea$Albacore[1,10,,1,] ,
Hist@FDead$Albacore[1, 10,,])


LoadArgs(Simulate_om)


# optimize depletion
# ref yield ..


Hist <- Simulate(testOM)



PopulateCatchObs





sim <- 2
y <- 40
cbind(Hist@FDead$Albacore[sim,,y,1], 
      Hist@FDeadArea$Albacore[sim,,y,1,])


# overall
N_Area <- Hist@Number$Albacore[sim,,y,]
B_Area <- N_Area * matrix(Hist@OM@Stock$Albacore@Weight@MeanAtAge[sim,,y], 16, 2)
M <- Hist@OM@Stock$Albacore@NaturalMortality@MeanAtAge[sim,,y]
Foverall <- Hist@FDead$Albacore[sim,,y,1]
Farea <- Hist@FDeadArea$Albacore[sim,,y,1,]

Z_overall <- M + Foverall
Z_area <- Farea + M

sum(Foverall/Z_overall * rowSums(B_Area) * (1-exp(-Z_overall)))
colSums(Farea/Z_area * B_Area * (1-exp(-Z_area))) |> sum()




stop()


