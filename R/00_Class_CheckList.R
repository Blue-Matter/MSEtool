
# Clashes with Slick - need new object class name

# #' @include 00_Class_unions.R
# 
# setClass("CheckList",
#          slots=c(object='character',
#                  errors='list',
#                  warnings='list',
#                  messages='list',
#                  complete='logical_list',
#                  populated='logical_list',
#                  empty='logical_list')
# )
# 
# setMethod("initialize", "CheckList", function(.Object) {
#   .Object@empty <- TRUE
#   .Object@complete <- FALSE
#   .Object@populated <- FALSE
#   .Object@empty <- TRUE
#   .Object
# })
# 
# CheckList <- function(object) {
#   ll <- new('CheckList')
#   ll@object <- class(object)
#   ll@populated <- as.logical(PopulatedObject(object))
#   ll@empty <- as.logical(EmptyObject(object))
#   ll
# }
# 