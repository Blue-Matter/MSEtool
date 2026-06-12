
library(MSEtool)


## ---- dome-create-fleet ----
DomeExFleet <- Fleet(Name = "DomeExFleet")


## ---- dome-stochastic effort ----
Effort(DomeExFleet) <- Effort(
  Effort = data.frame(
    Year  = c(0, 0.3, 0.6, 1.0),
    Lower = c(0, 0.4, 0.4, 1),
    Upper = c(0, 0.6, 0.6, 1),
    CV    = 0.1
  )
)


## ---- dome-selectivity ----
Selectivity(DomeExFleet) <- Selectivity(
  Pars = list(
    L5      = c(0.2, 0.4),
    LFS     = c(0.75, 1.1),
    Vmaxlen = c(0.5, 1)
  ), 
  isRel = TRUE
)

## ---- fig-dome-selectivity ----

# Populate Maturity and Selectivity objects 
# (Length and Weight objects are populated internally)

Maturity(AlbacoreExStock) <- PopulateMaturity(
  Maturity = Maturity(AlbacoreExStock),
  Ages     = Ages(AlbacoreExStock),
  Length   = Length(AlbacoreExStock),
  Weight   = Weight(AlbacoreExStock)
)

Selectivity <- PopulateSelectivity(
  Selectivity = Selectivity(DomeExFleet),
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
usethis::use_data(DomeExFleet, overwrite = TRUE)
