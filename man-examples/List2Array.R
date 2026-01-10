
x <- list(
  Cod = array(
    1:6,
    dim = c(2, 3),
    dimnames = list(
      Sim  = 1:2,
      Year = 2023:2025
    )
  ),
  Haddock = array(
    7:12,
    dim = c(2, 3),
    dimnames = list(
      Sim  = 1:2,
      Year = 2023:2025
    )
  )
)

array <- List2Array(x, name = "Stock", pos = 2)
dimnames(array)

Array2List(array) # convert back to x
