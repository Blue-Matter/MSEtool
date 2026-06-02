
## Compare stock–recruitment relationships

R0 <- 1000
S0 <- 1000
h <- 0.8
Shinge <- 0.2
S <- seq(0, S0, length.out = 200)

R_bh <- BevertonHolt(S, S0, R0, h)
R_rk <- Ricker(S, S0, R0, hR = h)
R_hs <- HockeyStick(S, S0, R0, Shinge)

plot(S, R_bh, type = "l", lwd = 2,
     ylim = c(0, 1200), xlab = "Spawning biomass (S)",
     ylab = "Recruitment")
lines(S, R_rk, lwd = 2, lty = 2)
lines(S, R_hs, lwd = 2, lty = 3)
abline(h=h*R0, lty=3)
abline(v=0.2*S0, lty=3)
legend("bottomright",
       legend = c("Beverton–Holt", "Ricker", "Hockey Stick"),
       lty = c(1, 2, 3), lwd = 2, bty = "n")


## Compare relative equilibrium recruitment vs SPR
SPR <- seq(0, 1, length.out = 200)

Pars_bh <- list(h = h)
Pars_rk <- list(hR = h)
Pars_hs <- list(Shinge = Shinge)

RR_bh <- sapply(SPR, BevertonHolt_RelRec, Pars = Pars_bh)
RR_rk <- sapply(SPR, Ricker_RelRec, Pars = Pars_rk)
RR_hs <- sapply(SPR, HockeyStick_RelRec, Pars = Pars_hs)

plot(SPR, RR_bh, type = "l", lwd = 2,
     ylim = c(0, 1.05), xlab = "SPR",
     ylab = "Relative recruitment (R / R0)")
lines(SPR, RR_rk, lwd = 2, lty = 2)
lines(SPR, RR_hs, lwd = 2, lty = 3)
legend("bottomright",
       legend = c("Beverton–Holt", "Ricker", "Hockey Stick"),
       lty = c(1, 2, 3), lwd = 2, bty = "n")


