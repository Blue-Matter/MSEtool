# #' Calculate Maximum Effort to Reach Target FInteract
# #'
# #' Iteratively scales fleet effort so that the maximum FInteract across stocks
# #' reaches the target maxF
# #'
# #' @param Proj An Hist object 
# #' @param sim Integer, simulation index
# #' @param TSIndex Integer, time-step index
# #' @param stocks Integer vector, stocks to include
# #' @param Year Numeric vector, year(s) to run
# #' @param tol Numeric, relative tolerance for convergence (default 1e-3)
# #' @param maxIter Integer, maximum number of iterations (default 10)
# #'
# #' @return Numeric vector of maximum effort by fleet
# #' @keywords internal
# CalcMaxEffort <- function(Proj, sim, TSIndex, stocks, 
#                           Year, TAC_by_Fleet,
#                           tol = 1e-2, maxIter = 10) {
#   
#   # Initial guess based on maxF and catchability
#   maxF <- Proj@OM@maxF
#   CurrQ <- Proj@Misc$Catchability[sim, stocks, TSIndex,, drop = FALSE] |>
#     abind::adrop(c(1, 3))
#   
#   MaxEffort_guess <- pmin(apply(maxF / CurrQ, 2, min), 1e6)
#   MaxEffort_guess <- MaxEffort_guess * TAC_by_Fleet/sum(TAC_by_Fleet)
#   Fprev <- -Inf
#   
#   # Iterative scaling
#   MaxEffort <- MaxEffort_guess
#   for (it in seq_len(maxIter)) {
#     
#     Proj@Effort[sim, TSIndex, ] <- MaxEffort
#     Temp_max <- CalcFisheryDynamics(Hist = Proj, Years = Year, Sims = sim)
#     
#     AllLandings <- sum(Temp_max@Landings[sim, stocks, TSIndex,])
#     if (AllLandings < 1E-3)
#       next()
#     
#     B0 <- Temp_max@Unfished@Equilibrium@Biomass
#     B0_sim <- min(TSIndex, dim(B0)[1])
#     B0 <- B0[B0_sim,stocks,, drop=FALSE] |> abind::adrop(1)
#     tind <- min(TSIndex, ncol(B0))
#     B_B0 <- min(Temp_max@Biomass[sim,stocks,TSIndex]/B0[,tind])
#     
#     if (B_B0 < 1E-3)
#       next()
#   
#     # Maximum FInteract across stocks
#     Fcurr <- rowSums(Temp_max@FInteract[sim, stocks, TSIndex, , drop = FALSE]) |> max()
#     
#     if (!is.finite(Fcurr) || Fcurr <= 0)
#       break()
#     
#     if (Fcurr <= Fprev * (1 + tol))
#       break()
#     
#     
#     if (abs(Fcurr - maxF) / maxF < tol) break
#     
#     Fprev <- Fcurr
#     
#     # Scale effort
#     MaxEffort <- MaxEffort * (maxF / Fcurr)
#   }
#   
#   return(MaxEffort)
# }
# 