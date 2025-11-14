# 
# OMListSimSubsetYear <- function(List, Years=NULL) {
#   nms <- names(List)
#   if (is.null(nms)) {
#     if (inherits(List, 'array')) {
#       if ("Year" %in% names(dimnames(List))) {
#         List <- ArraySubsetYear(List, Years)
#       }
#     }
#   } else {
#     for (i in seq_along(nms)) {
#       nmsList <- names(List[[i]]) 
#       if (isS4(List[[i]])) {
#         return(List[[i]])
#       }
#       
#       hasTS <- "Year" %in% nmsList
#       if (all(is.na(nmsList)))
#         nmsList <- NULL 
#       if (inherits(List[[i]], 'array')) {
#         if ("Year" %in% names(dimnames(List[[i]]))) {
#           List[[i]] <- ArraySubsetYear(List[[i]], Years)
#         }
#       } else if (is.list(List[[i]])) {
#         for (j in seq_along(List[[i]])) {
#           obj <- Recall(List[[i]][[j]], Years)
#           if (is.null(obj))
#             next()
#           List[[i]][[j]] <- Recall(List[[i]][[j]], Years)
#         }
#       } else if (inherits(List[[i]], 'numeric')) {
#         if (hasTS)
#           List[[i]] <- ArraySubsetYear(List[[i]], Years)
#       } else if (inherits(List[[i]], 'integer')) {
#         if (hasTS)
#           List[[i]] <-  ArraySubsetYear(List[[i]], Years)
#       }
#     }
#   }
#   List
# }
# 
# 
# CheckSimsUnique <- function(OMList, Years=NULL, ignore=c('Sim')) {
#   if (is.null(Years)) {
#     l1 <- OMList[[1]]
#     l2 <- OMList[[2]]
#   } else {
#     l1 <- OMListSimSubsetYear(OMList[[1]], Years)
#     l2 <- OMListSimSubsetYear(OMList[[2]], Years)  
#   }
#   
#   for (nm in ignore) {
#     l1[[nm]] <- NULL
#     l2[[nm]] <- NULL
#   }
#   digest::digest(l1, algo='spookyhash') == digest::digest(l2, algo='spookyhash')
# }




