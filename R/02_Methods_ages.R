#' @include 00_Class_ages.R
#' @include 00_Class_stock.R
#' @include 01_Generic_ages.R
#' 

#' @rdname Ages
setMethod("Ages", signature(MaxAge = "missing"),
  function(MaxAge, ...) {
    methods::new("ages")
  }
)

#' @rdname Ages
setMethod("Ages", signature(MaxAge = "numeric"),
          function(MaxAge,
                   MinAge = 0,
                   Units = "year",
                   PlusGroup = TRUE,
                   ...) {
            object <- methods::new('ages',
                                MaxAge=MaxAge,
                                MinAge=MinAge,
                                Units=Units,
                                PlusGroup=PlusGroup)
            
            validobjectect(object)
            object
          }
)

#' @rdname Ages
setMethod("Ages", signature(MaxAge = "stock"),
          function(MaxAge, ...) {
            MaxAge@Ages
          }
)

#' @rdname Ages
setReplaceMethod("Ages",signature(x = "stock", value = "ages"),
                 function(x, value) {
                   assignSlot(x, value, 'Ages')
                 }
)
