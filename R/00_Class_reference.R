
#' @include 00_Class_refpointsMSY.R 

 
setClass("reference",
         slots=c(SPR0='array.list.null',
                 MSY='refpointsMSY',
                 RefLandings='array.list.null',
                 RefRemovals='array.list.null',
                 
                 F01='array.list.null',
                 FMax='array.list.null',
                 FCrash='array.list.null',
                 SPRcrash='array.list.null',
                 MGT='array.list.null',
                 BLow='array.list.null',
                 Equilibrium='popdynamics',
                 Dynamic='popdynamics',
                 
                 Misc='list'
         )
)