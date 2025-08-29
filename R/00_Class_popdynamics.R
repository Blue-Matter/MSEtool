#' @include 00_Class_unions.R

setClass("popdynamics",
         slots=c(Number='array.list.null',
                 Biomass='array.list.null',
                 SBiomass='array.list.null',
                 SProduction='array.list.null',
                 Misc='list'
         )
)