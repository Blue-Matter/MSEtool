
ApplyMPAdvice <- function(ProjSim, MP, Year, YearsHist, YearsProj, ManagementYears) {
  
  

  MPAdviceList_Previous <- GetPreviousMPAdvice(ProjSim)
  Complexes <- ProjSim@OM@Complexes
  MPData <- GetMPData(ProjSim, Year, YearsAll)
  CheckDataLength(MPData, Complexes)

  
  if (!Year %in% ManagementYears) {
    MPAdviceList <- MPAdviceList_Previous
  } else {
    MPAdviceList <- CalcAdvice(MP, MPData, Year, ProjSim)
  }
  
  ProjSim <- SaveMPAdvice(ProjSim, MPAdviceList, Year) 
  
  # loop over stocks/complexes
  for (st in seq_along(MPAdviceList)) {
    MPAdvice <- MPAdviceList[[st]]
    ProjSim@Data[[st]]@Misc <- MPAdvice@Misc
    ProjSim <- ProjSim |>
      MPLog(MP, MPAdvice, Year) |>
      SaveMPTAC(MPAdviceList, st, Year, YearsProj) 

  }
  
  ProjSim <- ProjSim |>
    
    
    
    
    
    
    UpdateSelectivity(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateRetention(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateDiscardMortality(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateEffort(MPAdviceList, MPAdviceList_Previous, Year, YearsHist, YearsProj) |>
    UpdateTAC(MPAdviceList, MPAdviceList_Previous, Year, YearsAll) 
   
  
  ProjSim
}









