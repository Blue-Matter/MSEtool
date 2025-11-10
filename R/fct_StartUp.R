

StartUp <- function(OM, nSim=NULL, silent=FALSE) {
  CheckClass(OM)
  # TODO                    
  if (!is.null(OM@SexPars@Herm))
    stop('Herm not done yet!')
  
  OM |> 
    ReduceNSim(nSim) |>
    PopulateOM() |>
    ConvertToList() # converts OM@Stock and OM@Fleet to lists
  
}