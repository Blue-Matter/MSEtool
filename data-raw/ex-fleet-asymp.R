library(MSEtool)

## ---- asymp-create-fleet ----
AsympExFleet <- Fleet(Name = "AsympExFleet")


## ---- asymp-stochastic-effort ----
Effort(AsympExFleet) <- Effort(
  Effort = data.frame(
    Year  = c(0, 0.3, 0.6, 1.0),
    Lower = c(0, 0.4, 1, 1),
    Upper = c(0, 0.6, 1, 1),
    CV    = 0.1)
)

## ---- fig-asymp-effort ----
EffortDF <- Effort(AsympExFleet) |> Effort()

EffortArray <- GenHistEffort(EffortDF, nSim = 10, 
                             Years = 1980:2025)

Array2DF(EffortArray) |>
  ggplot2::ggplot(ggplot2::aes(x = Year, y = Value, group = Sim)) +
  ggplot2::geom_line(alpha = 0.6, color = "darkblue") +
  ggplot2::labs(y = "Relative Effort", x = "Year") +
  ggplot2::theme_bw()


## ---- asymp-selectivity ----
Selectivity(AsympExFleet) <- Selectivity(
  Pars = list(
    L5      = c(0.4, 0.5),
    LFS     = c(0.7, 0.8),
    Vmaxlen = 1
  ), 
  isRel = TRUE
)

## ---- save object
usethis::use_data(AsympExFleet, overwrite = TRUE)
