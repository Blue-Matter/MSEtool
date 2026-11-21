# Extracted from test-tac-unreachable.R:110

# prequel ----------------------------------------------------------------------
BigTAC <- function(Data) Advice(TAC = 1e9)
class(BigTAC) <- 'mp'
.TACMP <- function(TAC) {
  mp <- eval(bquote(function(Data) Advice(TAC = .(TAC))))
  class(mp) <- 'mp'
  mp
}
.TotalF <- function(MSE, st = 1)
  apply(MSE@FInteract[, st, , , , drop = FALSE], c(1, 3, 5), sum)
.LastCatch <- function(Hist, st = 1) {
  ny <- dim(Hist@Landings)[3]
  apply(ArraySum(Hist@Landings, Hist@Discards)[, st, ny, , drop = FALSE], c(1, 4), sum)
}
.EffortConvergenceLog <- function(MSE)
  Filter(\(e) identical(e$name, 'EffortConvergence'), MSE@Log$warning)

# test -------------------------------------------------------------------------
skip_on_cran()
om <- TwoFleetOM
nSim(om) <- 3
pYear(om) <- 2
Hist <- Simulate(om, silent = TRUE)
Alone <- TACAttainment(Project(Hist, MPs = list(OneFleet = .TACMP(c(1e9, 0))),
                                 parallel = FALSE, silent = TRUE))
Alone <- Alone[Alone$Fleet == FleetNames(Hist)[1] & Alone$Year == min(Alone$Year), ]
TAC2  <- 0.1 * mean(.LastCatch(Hist)[, 2])
assign('.TACAlone', cbind(0.8 * Alone$Catch[order(Alone$Sim)], TAC2), globalenv())
on.exit(rm('.TACAlone', envir = globalenv()), add = TRUE)
Near <- function(Data) Advice(TAC = .TACAlone[Data@Misc$Sim, ])
class(Near) <- 'mp'
MSE <- Project(Hist, MPs = list(Near = Near), parallel = FALSE, silent = TRUE)
df  <- TACAttainment(MSE)
df  <- df[df$Year == min(df$Year), ]
expect_true(all(df$Limit == 'None'))
expect_equal(df$Catch, df$TAC, tolerance = 1e-3)
expect_length(.EffortConvergenceLog(MSE), 0)
