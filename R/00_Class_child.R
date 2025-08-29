#' @include 00_Class_unions.R



# Child classes ----
setClass("ParsClassNoUnits",
         slots=c(Pars='list',
                 Model='fun.char'
         )
)

setClass("ParsClass",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null'
         )
)

setClass("DistClass",
         slots=c(CVatAge='num.array',
                 Dist='character',
                 TruncSD='num.null'
         )
)

setClass("MeanAtAgeClass",
         slots=c(MeanAtAge='num.array')
)


setClass("MeanAtLengthClass",
         slots=c(MeanAtLength='num.array.list')
)

setClass("MeanAtWeightClass",
         slots=c(MeanAtWeight='num.array.list')
)

setClass("RandomClass",
         slots=c(Random='num.array')
)

setClass("ASKClass",
         slots=c(ASK='ASK.null')
)

setClass("ClassesClass",
         slots=c(Classes='num.list.null')
)

setClass("SemelparousClass",
         slots=c(Semelparous='logical.array')
)

setClass("Timing",
         slots=c(Timing='num.null')
)

setClass("MiscClass",
         slots=c(Misc='list')
)


