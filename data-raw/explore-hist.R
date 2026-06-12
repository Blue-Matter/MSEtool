library(MSEtool)

## ---- run-hist ----
Hist <- Simulate(SingleStockOM, silent = TRUE)

## ---- explore-hist-biomass ----
Biomass(Hist) |> head()

SBiomass(Hist) |> head()

## ---- fig-hist-biomass ----
dplyr::bind_rows(
  Biomass(Hist),
  SBiomass(Hist)
) |>
  ggplot2::ggplot(ggplot2::aes(x = Year, y = Value, group = Sim)) +
  ggplot2::geom_line(alpha = 0.6, colour = "steelblue") +
  ggplot2::facet_wrap(~ Variable, scales = "free_y") +
  ggplot2::labs(y = "Biomass", x = "Year") +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                               limits = c(0, NA)) +
  ggplot2::theme_bw()

## ---- explore-hist-catch ----
Landings(Hist) |> head()

Discards(Hist) |> head()

## ---- fig-hist-catch ----
dplyr::bind_rows(
  Landings(Hist),
  Discards(Hist)
) |>
  ggplot2::ggplot(ggplot2::aes(x = Year, y = Value, group = Sim)) +
  ggplot2::geom_line(alpha = 0.6, colour = "darkred") +
  ggplot2::facet_wrap(~ Variable, scales = "free_y") +
  ggplot2::labs(y = "Catch (biomass)", x = "Year") +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                               limits = c(0, NA)) +
  ggplot2::theme_bw()

## ---- fig-hist-F ----
FDead(Hist) |>
  ggplot2::ggplot(ggplot2::aes(x = Year, y = Value, group = Sim)) +
  ggplot2::geom_line(alpha = 0.6, colour = "coral") +
  ggplot2::labs(y = "Apical fishing mortality (F)", x = "Year") +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                               limits = c(0, NA)) +
  ggplot2::theme_bw()
