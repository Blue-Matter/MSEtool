#' @include 00_Class_popdynamics.R

setClass("refpoints",
         slots=c(Curves='curves',
                 SPR0='array.list.null',
                 MSY='array.list.null',
                 FMSY='array.list.null',
                 BMSY='array.list.null',
                 SBMSY='array.list.null',
                 SPMSY='array.list.null',
                 SPRMSY='array.list.null',
                 F01='array.list.null',
                 FMax='array.list.null',
                 FCrash='array.list.null',
                 SPRcrash='array.list.null',
                 MGT='array.list.null',
                 RefYield='array.list.null',
                 BLow='array.list.null',
                 Equilibrium='popdynamics',
                 Dynamic='popdynamics',
                 Misc='list'
         )
)