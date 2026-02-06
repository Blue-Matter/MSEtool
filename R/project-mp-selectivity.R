Update_Selectivity <- function(Proj, Year, AdviceSimList, LastAdviceSimList,
                               YearsProj, Areas, FleetNames,
                               type=c('Selectivity', 'Retention')) {
  
  type <- match.arg(type, c('Selectivity', 'Retention'))
  
  for (sim in seq_len(nSim)) {
    Proj <- Update_Selectivity_Sim(
      Proj = Proj,
      sim = sim,
      Year = Year,
      YearsProj = YearsProj,
      AdviceList = AdviceSimList[[sim]],
      LastAdviceList = LastAdviceSimList[[sim]],
      FleetNames = FleetNames,
      Complexes = Proj@OM@Complexes,
      Areas = Areas,
      nSim = Proj@OM@nSim,
      type = tpye
    )
  }
  
  Proj
}






Update_Retention <- function(Proj, Year, AdviceSimList, LastAdviceSimList,
                             YearsProj, Areas, FleetNames) {
  
  Update_Selectivity(Proj, Year, AdviceSimList, LastAdviceSimList, 
                     YearsProj, Areas, FleetNames, 
                     type='Retention')
  
}