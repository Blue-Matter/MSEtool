

StartUp <- function(OM, nSim=NULL, silent=FALSE) {
  CheckClass(OM)
  # TODO                    
  if (!is.null(OM@Herm))
    stop('Herm not done yet!')
  
  OM |> 
    PopulateOM() |>
    ReduceNSim(nSim) |>
    ConvertToList() # converts OM@Stock and OM@Fleet to lists
  
}