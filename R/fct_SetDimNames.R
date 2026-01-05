
# Sim, Age, Year, Area
SetDimNames_SAYR <- function(array, AgeClasses, Years=NULL) {
  if (is.null(array)) {
    return(array)
  }
  Years <- DefaultYears(Years) 
  dnames <- dimnames(array)
  
  if (is.null(dnames)) {
    dnames <- list(Sim=NA, Age=NA, Year=NA, Area=NA)
  }
  dd <- dim(array)
  
  dnames[['Sim']] <- 1:dd[1]
  dnames[['Age']] <- AgeClasses
  if (any(!is.finite(dnames[['Year']]))) {
    dnames[['Year']] <- Years[1:dd[3]]
  }
  
  dnames[['Area']] <- 1:dd[4]
  
  dimnames(array) <- dnames
  array
}

# Sim, Class, Year, Area
SetDimNames_SCYR <- function(array, Classes, Years=NULL) {
  if (is.null(array)) {
    return(array)
  }
  Years <- DefaultYears(Years) 
  dnames <- dimnames(array)
  
  if (is.null(dnames)) {
    dnames <- list(Sim=NA, Class=NA, Year=NA, Area=NA)
  }
  dd <- dim(array)
  
  dnames[['Sim']] <- 1:dd[1]
  dnames[['Class']] <- Classes
  if (any(!is.finite(dnames[['Year']]))) {
    dnames[['Year']] <- Years[1:dd[3]]
  }
  
  dnames[['Area']] <- 1:dd[4]
  dimnames(array) <- dnames
  array
}