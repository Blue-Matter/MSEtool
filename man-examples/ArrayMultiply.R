
# Array with Sim, Year 
array1 <- array(1:20, 
                dim=c(2,5),
                dimnames = list(
                  Sim=1:2,
                  Year=2021:2025
                ))

array2 <- array(1:20, 
                dim=c(1,3),
                dimnames = list(
                  Sim=1,
                  Year=2024:2026
                ))

ArrayExtend(array1, array2)

ArraySum(array1, array2)
ArrayDivide(array1, array2)
ArrayMultiply(array1, array2)
ArraySubtract(array1, array2)



# Array with Age, Year 
array1 <- array(1:20, 
                dim=c(5,2),
                dimnames = list(
                  Age=1:5,
                  Year=2021:2022
                ))

array2 <- array(1:20, 
                dim=c(1,3),
                dimnames = list(
                  Age=1,
                  Year=2024:2026
                ))

ArrayExtend(array1, array2)

ArraySum(array1, array2)
ArrayDivide(array1, array2)
ArrayMultiply(array1, array2)
ArraySubtract(array1, array2)


# ArrayFill

## Same dimensions
object <- array(NA, 
               dim=c(3,5),
               dimnames=list(Sim=1:3,
                             Year=2021:2025)
)

value <- array(1:2, 
               dim=c(1,2),
               dimnames=list(Sim=2,
                             Year=2022:2023)
)

ArrayFill(object) <- value
object


## Extend Dimensions
object <- array(NA, 
                dim=c(3,5),
                dimnames=list(Sim=1:3,
                              Year=2021:2025)
)

value <- array(1:12, 
               dim=c(3,4),
               dimnames=list(Sim=1:3,
                             Year=c(2019, 2025:2027))
)

ArrayFill(object) <- value
object
