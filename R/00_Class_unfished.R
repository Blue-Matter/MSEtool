#' @include 00_Class_popdynamics.R

setClass("unfished",
         slots=c(Equilibrium='popdynamics',
                 Dynamic='popdynamics',
                 Misc='list'
         )
)