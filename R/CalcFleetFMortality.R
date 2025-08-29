# CalcFleetFMortality <- function(Hist, TimeSteps=NULL) {
#   # calculates overall F by fleet for the given TimeStep(s)
#   # and updates FishingMortality@ApicalF, DeadAtAge, and RetainAtAge
#   # for each fleet for these timesteps 
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- TimeSteps(Hist)
#   
#   # Calculate overall apical F by fleet (ie over all areas)
#   CatchDeadAtAge <- GetRemovalAtAge(Hist,TimeSteps=TimeSteps) |>
#     purrr::map(\(x) {
#       apply(x, c('Sim', 'Age', 'Time Step', 'Fleet'), sum)
#     })
#   
#   BiomassAtAge <- GetBiomassAtAge(Hist, TimeSteps=TimeSteps) |>
#     purrr::map(\(x) {
#       apply(x, c('Sim', 'Age', 'Time Step'), sum)
#     })
#   
#   NMortalityAtAge <- purrr::map(Hist@Stock, 
#                                 GetNMortalityAtAge, TimeSteps=TimeSteps)
#   SelectAtAge <- purrr::map(Hist@Fleet, 
#                             GetSelectivityAtAge, TimeSteps=TimeSteps) 
#   RetainAtAge <- purrr::map(Hist@Fleet, 
#                             GetRetentionAtAge, TimeSteps=TimeSteps) 
#   DiscardAtAge <- purrr::map(Hist@Fleet,
#                              GetDiscardMortalityAtAge, TimeSteps=TimeSteps) 
#   
#   ApicalF <- purrr::pmap(list(CatchAtAge=CatchDeadAtAge, 
#                               PopatAge=BiomassAtAge, 
#                               NMortalityAtAge, 
#                               SelectAtAge,
#                               RetainAtAge,
#                               DiscardAtAge
#   ),
#   CalcFfromCatch) |>
#     purrr::map(Array2List)
#   
#   
#   Hist@Fleet <- purrr::map2(Hist@Fleet, ApicalF, \(x,y)
#                             CalcFatAge(x, TimeSteps=TimeSteps, y)
#   )
#   Hist
# }
# 
# CatchAtAge=CatchDeadAtAge$Albacore 
# PopatAge=BiomassAtAge$Albacore
# NMortalityAtAge=NMortalityAtAge$Albacore
# SelectAtAge=SelectAtAge$Albacore
# RetainAtAge=RetainAtAge$Albacore
# DiscardAtAge=DiscardAtAge$Albacore

