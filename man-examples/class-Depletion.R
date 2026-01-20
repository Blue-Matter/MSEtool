
# Empty `Depletion` object
myDep <- Depletion()

# Set Initial depletion
#
# Note: Populate() is usually called internally. Included here for demonstrating final object.

## Case 1: Constant over simulations
myDep <- Depletion(Initial=0.8) |> Populate()
Initial(myDep)

# equivalent:
Initial(myDep) <- 0.1
Populate(myDep)

## Case 2: `nSim` values
Depletion(Initial=c(0.6, 0.7, 0.8, 0.9, 1)) |> Populate(nSim=5)


## Case 3: Stochastic sample from uniform distribution
Depletion(Initial=c(0.6, 0.9)) |> Populate(nSim=30)

