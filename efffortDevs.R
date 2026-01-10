
# This is the way .... 
B_overall <- 100 
E_overall <- 5
q <- 0.001

Area_Size <- c(0.1, 0.2, 0.3, 0.4)

# biomass
B_area <- B_overall * (Area_Size)
relB <- B_area/sum(B_area)

target <- exp(relB)/sum(exp(relB))

# total effort in each area (sums to E_overall) 

E_area_total <- E_overall * target

# effort density (same everywhere) 
E_density <- E_area_total / Area_Size

# fishing mortality 
F_area <- q * E_density


# catch 
C_area <- F_area * B_area

sum(C_area) == q * E_density[1] * B_overall
