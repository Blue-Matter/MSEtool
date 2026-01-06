# If `Movement` is provided, everything else (except RelativeSize) is calculated.
# Otherwise, `Movement` is calculated 


# Spatial Age Structure

# No Spatial Structure
Spatial <- Spatial() |> Populate()

UnfishedDist(Spatial) |> dimnames()
ProbStaying(Spatial) |> dimnames()
RelativeSize(Spatial) |> dimnames()
Movement(Spatial) |> dimnames()

# Two Area - deterministic
Spatial <- Spatial(UnfishedDist=0.3, # Fraction Unfished in Area 1
                   ProbStaying=0.6, # Prob Staying in Area 1 
                   RelativeSize=0.8)  |> # Relative Size of Area 1  
  Populate()

UnfishedDist(Spatial)
ProbStaying(Spatial)
RelativeSize(Spatial)
Movement(Spatial)

Spatial <- Spatial(Movement=Movement(Spatial))


# Two Area - Stochastic 
Spatial <- Spatial(UnfishedDist=c(0.095, 0.105),
                   ProbStaying=c(0.8, 0.9),
                   RelativeSize=c(0.095, 0.105)) |>
  Populate()

UnfishedDist(Spatial)
ProbStaying(Spatial)
RelativeSize(Spatial)
Movement(Spatial)





# Equal Density ... 

Spatial <- Spatial(ExampleStock) 


Spatial <- PopulateSpatial(Spatial)

object <- Spatial


PopulateSpatial()

PopulateSpatial()
ExampleStock

# movement by age - move to area 2 as they age

# Four Areas 

# move through areas .... 





OM <- ExampleOM |> Populate()


# test from to movement patterns 


# Seasonal-Spatial Age Structure 

# Seasonal Migration ...



# Then:
# - initialize age-structure...
# - continue with Simulate_om 