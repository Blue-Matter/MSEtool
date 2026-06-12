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

# Extract `Effort` slot from the `Effort` object
EffortDF <- Effort(AsympExFleet) |> Effort()

# Generate effort trajectories 
EffortArray <- GenHistEffort(EffortDF,
                             nSim  = 10, 
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

## ---- fig-asymp-selectivity ----

# Populate Maturity and Selectivity objects 
# (Length and Weight objects are populated internally)

Maturity(AlbacoreExStock) <- PopulateMaturity(
  Maturity = Maturity(AlbacoreExStock),
  Ages     = Ages(AlbacoreExStock),
  Length   = Length(AlbacoreExStock),
  Weight   = Weight(AlbacoreExStock)
)

Selectivity <- PopulateSelectivity(
  Selectivity = Selectivity(AsympExFleet),
  Ages        = Ages(AlbacoreExStock),
  Length      = Length(AlbacoreExStock),
  Weight      = Weight(AlbacoreExStock),
  Maturity    = Maturity(AlbacoreExStock)
)

# Build combined data frames 
sel_age <- MeanAtAge(Selectivity) |>
  Array2DF() |>
  dplyr::mutate(Schedule = "Selectivity")

mat_age <- MeanAtAge(Maturity(AlbacoreExStock)) |>
  Array2DF() |>
  dplyr::mutate(Schedule = "Maturity")

sel_len <- MeanAtLength(Selectivity) |>
  Array2DF() |>
  dplyr::mutate(Schedule = "Selectivity")

mat_len <- MeanAtLength(Maturity(AlbacoreExStock)) |>
  Array2DF() |>
  dplyr::mutate(Schedule = "Maturity")

df_age <- dplyr::bind_rows(sel_age, mat_age) 
df_len <- dplyr::bind_rows(sel_len, mat_len) 

# Plot schedules
cols <- c("Selectivity" = "steelblue", "Maturity" = "coral")

p_len <- ggplot2::ggplot(df_len,
                         ggplot2::aes(x = Class,
                                      y = Value, 
                                      colour = Schedule, 
                                      group = interaction(Sim, Schedule))) +
  ggplot2::geom_line(alpha = 0.8) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Length (cm)", y = 'Probability') +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.02)), 
    limits = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

p_age <- ggplot2::ggplot(df_age,
                         ggplot2::aes(x = Age,
                                      y = Value, 
                                      colour = Schedule, 
                                      group = interaction(Sim, Schedule))) +
  ggplot2::geom_line(alpha = 0.8) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Age", y = NULL) +
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.02)), 
    limits = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())
  
patchwork::wrap_plots(p_len, p_age, ncol = 2) 


## ---- save object
usethis::use_data(AsympExFleet, overwrite = TRUE)




