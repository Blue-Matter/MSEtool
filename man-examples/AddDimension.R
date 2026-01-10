
array <- array(1:6, dim=c(1,2,3),
               dimnames = list(
                 Sim=1,
                 Age=0:1,
                 Year=2023:2025
               ))

AddDimension(array, c('Test', 'best'),pos=4)


DropDimension(array, 'Sim')
DropDimension(array, c('Sim', 'Age'))
DropDimension(array, c('Age', 'Year'), warn=FALSE)
