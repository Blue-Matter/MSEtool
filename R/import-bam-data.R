# # Units for catches? Number vs Biomass ??
# # add as an argument and provide manually ...
# 
# ImportBAMData <- function(OM, BAMdata) {
#   
# 
#   data <- Data(Name=OM@Stock[[1]]@Name) 
#   
#   fleet.names <- FleetNames(OM)
#   
#   # Time-series
#   t.series <- BAMdata$t.series
#   years <- t.series$year
#   n.year <- length(years)
#   cnames <- colnames(t.series)
#   obs.names <- cnames[grepl('\\.ob', cnames)]
#   
#   # Indices 
#   indices.names <- obs.names[grepl("^U\\.", obs.names)]
# 
#   # CPUE 
#   cpue.names <- NULL
#   for (i in seq_along(fleet.names)) 
#     cpue.names<- c(cpue.names, indices.names[grepl(fleet.names[i], indices.names)])
#   
#   not.disc.fleets <- !grepl("\\.D.", cpue.names) # drop discard fleets
#   cpue.names <- cpue.names[not.disc.fleets]
#   n.cpue <- length(cpue.names)
#     
#   if (n.cpue) {
#     CPUE <- IndicesData(Name=cpue.names)
#     
#     data@CPUE@Value <- array(NA, c(n.cpue, n.year), 
#                              dimnames = list(Fleet = cpue.names,
#                                              Year  = years 
#                              ))
#     data@CPUE@CV
#     data@CPUE@Units
#     
#   }
#   
#   
#   
# 
#   
# 
#   
#   startsWith("U.", cnames)
#   
#  
#   
#   # Fleets
#   for (i in seq_along(fleet.names)) {
#     fleet.t.series.names <- obs.names[grepl(fleet.names[i], obs.names)]  
#     if (!length(fleet.t.series.names)) next
#     
#     index.name <- fleet.t.series.names[grepl("U\\.", fleet.t.series.names)]
#     
#     # CPUE Index
#     if (length(index.name)) {
#       Index <- t.series[[index.name]]
#       cv.name <- paste0('cv.', gsub('.ob', '', index.name))
#       CV <- t.series[[cv.name]]
#       
#       
#     
#       
#     }
#     
#     t.series$cv.U.cHL 
#   }
#   
#   
#   
#   # Surveys
#   
#   
#   
#   
#   
#   BAMdata$info$units.landings
#   BAMdata$t.serie
#   
#   plot(BAMdata$t.series$L.rGN.ob) 
#   
#   BAMdata$t.series$U.cHL.ob
#   
#   BAMdata$t.series$U.sCT.pr
#   
# 
#   
#   # Discards
#   
#   # Indices 
#   
#   # Composition - Age
#   
#   # Composition - Length 
#   BAMdata$comp.mats$lcomp.cHL.ob |> head()
#   
#   
#   data
# }