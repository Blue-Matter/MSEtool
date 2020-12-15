library(MSEtool)
funs <- ls("package:MSEtool")

DF <- data.frame(Object=funs)
DF$Class <- DF$File <- DF$Family <- NA

for (i in 1:nrow(DF)) {
  DF$Class[i] <- class(get(DF$Object[i]))
  
  srcfile <- attr(attr(get(DF$Object[i]),"srcref"),"srcfile")
  if (!is.null(srcfile)) DF$File[i] <- basename(srcfile$filename)
  
  
  class <- DF$Class[i]
  if (class %in% c('Stock', 'Fleet', 'Obs', 'Imp', 'Data', 'MOM')) {
    DF$Family[i] <- paste('Example', class, 'Objects')
  }

  if (class %in% c('PM')) {
    DF$Family[i] <- paste('Example', class, 'Methods')
  }
  
  if (class %in% c('MP')) {
    DF$Family[i] <- 'Reference MPs'
  }
}

openxlsx::write.xlsx(DF, 'build_tools/exported.xlsx')

# Compare with build_tools/Exported_Functions.xlsx



