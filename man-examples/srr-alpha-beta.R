ab <- SRRAlphaBeta(h = 0.7, R0 = 1000, phi0 = 2.5)
ab
SRRSteepness(ab$alpha, ab$beta, phi0 = 2.5)

# Specify a Beverton-Holt SRR directly with alpha and beta
SRR(Pars = list(alpha = ab$alpha, beta = ab$beta), Model = "BevertonHolt")
