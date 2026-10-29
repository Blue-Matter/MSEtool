# ---- Setup Array ----
Arr <- array(1:24, dim = c(2, 3, 4),
               dimnames = list(
                 Sim  = 1:2,
                 Age  = 0:2,
                 Year = 2020:2023
               ))

dim(Arr)

# ---- AddDimension ----

# Append an Area dimension at the end (default pos)
AddDimension(Arr, 'Area') |> Dim()

# Append an Area dimension with two values
AddDimension(Arr, 'Area', val = 1:2) |> Dim()

# Adding a dimension that already exists returns the array unchanged
AddDimension(Arr, 'Sim') |> Dim()

# ---- DropDimension ----

# Drop a length-1 dimension (no warning)
Arr <- array(1:12, dim = c(1, 3, 4),
           dimnames = list(Sim = 1, Age = 0:2, Year = 2020:2023))

DropDimension(Arr, 'Sim')

# Drop a length > 1 dimension (warns by default)
DropDimension(Arr, 'Age')

# Suppress the warning
DropDimension(Arr, 'Age', warn = FALSE)

# Drop multiple dimensions at once
DropDimension(Arr, c('Sim', 'Year'), warn = FALSE)

