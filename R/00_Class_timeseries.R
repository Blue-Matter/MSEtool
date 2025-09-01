#' @include 00_Class_unions.R

setClass("timeseries",
         slots=c(Number='array.list.null',
                 Biomass='array.list.null',
                 SBiomass='array.list.null',
                 SProduction='array.list.null',
                 Landings='array.list.null',
                 Discards='array.list.null',
                 Effort='array.list.null',
                 FDeadAtAge='array.list.null',
                 FRetainAtAge='array.list.null',
                 EffortArea='array.list.null',
                 FDeadAtAgeArea='array.list.null',
                 FRetainAtAgeArea='array.list.null',
                 Misc='list'
         )
)
