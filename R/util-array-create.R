#' Construct simulation arrays over common OM dimensions
#'
#' Internal helper functions for constructing arrays (and stock-indexed lists
#' of arrays) over common operating model dimensions such as simulation,
#' stock, age or class, year, fleet, area, and management procedure (MP).
#'
#' Each `Array*` function returns a single array with named dimensions, while
#' corresponding `ListArray*` functions return named lists indexed by stock.
#'
#' Dimension conventions:
#' * `Sim` — simulation index (`1:nSim`)
#' * `Stock` — stock names from OM metadata
#' * `Age` — age classes
#' * `Class` — selectivity classes defined by fleet selectivity
#' * `Year` — model years, optionally restricted via `Years`
#' * `Fleet` — fleet names from OM metadata
#' * `Area` — spatial areas (`1:nAreas`)
#' * `MP` — management procedure identifiers
#'
#' All arrays are initialized to a constant value supplied via `default`
#' and include fully named dimensions for downstream safety and clarity.
#'
#' These functions are not exported and are intended for internal use when
#' constructing or populating operating model components.
#'
#' @param OM An operating model object.
#' @param Period Character string indicating the model period
#'   (`"Historical"`, `"Projection"`, or `"All"`).
#' @param stock Integer stock index (for single-stock array constructors).
#' @param fleet Integer fleet index (for fleet-specific class arrays).
#' @param MPs Optional character vector of management procedure names.
#' @param Years Optional subset of model years.
#' @param default Scalar value used to initialize arrays.
#' 
#' @name internal_arrays
#' @keywords internal
NULL


#' @rdname internal_arrays
ArraySimStockAgeTimeArea <- function(
    Sims, 
    Stocks, 
    Ages, 
    Years, 
    Areas, 
    default = 0) {
  array(
    default,
    dim = c(length(Sims), length(Stocks), length(Ages), length(Years), length(Areas)),
    dimnames = list(
      Sim = Sims, 
      Stock = Stocks, 
      Age = Ages, 
      Year = Years, 
      Area = Areas
    )
  )
}

#' @rdname internal_arrays
ArraySimAge <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    stock = 1, 
    default = 0) {
  meta <- GetMetaData(OM, Period)
  nAges <- meta$nAges[[stock]]
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(meta$nSim, nAges),
    dimnames = list(
      Sim = 1:meta$nSim,
      Age = AgeClasses
    )
  )
}

#' @rdname internal_arrays
ListArraySimAge <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    default = 0) {
  meta <- GetMetaData(OM, Period)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAge(
      OM, 
      Period, 
      stock = st, 
      default = default
    )
  )
}

#' @rdname internal_arrays
ArraySimAgeTime <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  nAges <- meta$nAges[[stock]]
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(meta$nSim, nAges, length(meta$Years)),
    dimnames = list(
      Sim = 1:meta$nSim,
      Age = AgeClasses,
      Year = meta$Years
    )
  )
}

#' @rdname internal_arrays
ListArraySimAgeTime <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAgeTime(
      OM, 
      Period, 
      stock = st, 
      default = default, 
      Years = Years
    )
  )
}

#' @rdname internal_arrays
ArraySimAgeTimeArea <- function(
    OM, 
    Period = c("Historical", "Projection", "All"),
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(meta$nSim, length(AgeClasses), length(meta$Years), meta$nAreas),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Age = AgeClasses, 
      Year = meta$Years, 
      Area = 1:meta$nAreas
    )
  )
}

#' @rdname internal_arrays
ListArraySimAgeTimeArea <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAgeTimeArea(
      OM, 
      Period, 
      stock = st, 
      default = default, 
      Years = Years
    )
  )
}

#' @rdname internal_arrays
ArraySimAgeTimeFleet <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(meta$nSim, length(AgeClasses), length(meta$Years), length(meta$FleetNames)),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Age = AgeClasses, 
      Year = meta$Years, 
      Fleet = meta$FleetNames
    )
  )
}

#' @rdname internal_arrays
ListArraySimAgeTimeFleet <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAgeTimeFleet(
      OM, 
      Period, 
      stock = st, 
      default = default, 
      Years = Years
    )
  )
}


#' @rdname internal_arrays
ArraySimAgeTimeFleetArea <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(
      meta$nSim, 
      length(AgeClasses), 
      length(meta$Years), 
      length(meta$FleetNames), 
      meta$nAreas
    ),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Age = AgeClasses, 
      Year = meta$Years, 
      Fleet = meta$FleetNames, 
      Area = 1:meta$nAreas
    )
  )
}

#' @rdname internal_arrays
ListArraySimAgeTimeFleetArea <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAgeTimeFleetArea(
      OM, 
      Period, 
      stock = st, 
      default = default, 
      Years = Years
    )
  )
}

#' @rdname internal_arrays
ArraySimClassYearArea <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    stock = 1, 
    fleet = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  Classes <- OM@Fleet[[stock]][[fleet]]@Selectivity@Classes
  
  array(
    default,
    dim = c(meta$nSim, length(Classes), length(meta$Years), meta$nAreas),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Class = Classes, 
      Year = meta$Years, 
      Area = 1:meta$nAreas
    )
  )
}

#' @rdname internal_arrays
ListArraySimClassTimeFleetArea <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  .MakeStockList(
    meta$StockNames,
    function(st) {
      FleetNames <- meta$FleetNames
      List <- MakeNamedList(FleetNames)
      for (fl in seq_along(FleetNames)) {
        List[[fl]] <- ArraySimClassYearArea(
          OM, 
          Period, 
          stock = st, 
          fleet = fl, 
          default = default, 
          Years = Years
        )
      }
      List
    }
  )
}

#' @rdname internal_arrays
ArraySimAgeTimeMPArea <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period, Years)
  nAges <- meta$nAges[[stock]]
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(meta$nSim, nAges, length(meta$Years), meta$nAreas, length(MPs)),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Age = AgeClasses, 
      Year = meta$Years, 
      Area = 1:meta$nAreas, 
      MP = MPs
    )
  )
}

#' @rdname internal_arrays
ListArraySimAgeTimeAreaMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAgeTimeMPArea(
      OM, 
      Period, 
      MPs, 
      stock = st, 
      default = default, 
      Years = Years
    )
  )
}

#' @rdname internal_arrays
ArraySimAgeTimeFleetAreaMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  FleetNames <- meta$FleetNames
  nAges <- meta$nAges[[stock]]
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(
      meta$nSim, 
      nAges, 
      length(meta$Years), 
      length(FleetNames), 
      meta$nAreas, 
      length(MPs)
    ),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Age = AgeClasses, 
      Year = meta$Years, 
      Fleet = FleetNames, 
      Area = 1:meta$nAreas, 
      MP = MPs
    )
  )
}

#' @rdname internal_arrays
ListArraySimAgeTimeFleetAreaMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAgeTimeFleetAreaMP(
      OM, 
      Period, 
      MPs, 
      stock = st, 
      default = default, 
      Years = Years
    )
  )
}

#' @rdname internal_arrays
ArraySimClassYearAreaMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    stock = 1, 
    fleet = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  Classes <- OM@Fleet[[stock]][[fleet]]@Selectivity@Classes
  
  array(
    default,
    dim = c(meta$nSim, length(Classes), length(meta$Years), meta$nAreas, length(MPs)),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Class = Classes, 
      Year = meta$Years, 
      Area = 1:meta$nAreas, 
      MP = MPs
    )
  )
}

#' @rdname internal_arrays
ListArraySimClassTimeFleetAreaMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  .MakeStockList(
    meta$StockNames,
    function(st) {
      FleetNames <- meta$FleetNames
      List <- MakeNamedList(FleetNames)
      for (fl in seq_along(FleetNames)) {
        List[[fl]] <- ArraySimClassYearAreaMP(
          OM, 
          Period, 
          MPs, 
          stock = st, 
          fleet = fl, 
          default = default, 
          Years = Years
        )
      }
      List
    }
  )
}

#' @rdname internal_arrays
ArraySimStockTimeMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  StockNames <- meta$StockNames
  
  array(
    default,
    dim = c(meta$nSim, length(StockNames), length(meta$Years), length(MPs)),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Stock = StockNames, 
      Year = meta$Years, 
      MP = MPs
    )
  )
}

#' @rdname internal_arrays
ArraySimStockTimeFleetMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  FleetNames <- meta$FleetNames
  StockNames <- meta$StockNames
  
  array(
    default,
    dim = c(meta$nSim, length(StockNames), length(meta$Years), length(FleetNames), length(MPs)),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Stock = StockNames, 
      Year = meta$Years, 
      Fleet = FleetNames, 
      MP = MPs
    )
  )
}

#' @rdname internal_arrays
ArraySimAgeTimeFleetMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    stock = 1, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  FleetNames <- meta$FleetNames
  AgeClasses <- meta$AgeClasses[[stock]]
  
  array(
    default,
    dim = c(meta$nSim, length(AgeClasses), length(meta$Years), length(FleetNames), length(MPs)),
    dimnames = list(
      Sim = 1:meta$nSim, 
      Age = AgeClasses, 
      Year = meta$Years, 
      Fleet = FleetNames, 
      MP = MPs
    )
  )
}

#' @rdname internal_arrays
ListArraySimAgeTimeFleetMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  .MakeStockList(
    meta$StockNames,
    function(st) ArraySimAgeTimeFleetMP(
      OM, 
      Period, 
      MPs, 
      stock = st, 
      default = default, 
      Years = Years
    )
  )
}

#' @rdname internal_arrays
ListArraySimTimeFleetAreaMP <- function(
    OM, 
    Period = c("Historical", "Projection", "All"), 
    MPs = NULL, 
    default = 0, 
    Years = NULL) {
  meta <- GetMetaData(OM, Period)
  .MakeStockList(
    meta$StockNames,
    function(st) {
      ArraySimAgeTimeFleetAreaMP(
        OM, 
        Period, 
        MPs, 
        stock = st, 
        default = default, 
        Years = Years
      ) |>
        DropDimension("Age", FALSE)
    }
  )
}


.MakeStockList <- function(stocknames, FUN) {
  List <- MakeNamedList(stocknames)
  for (st in seq_along(stocknames)) {
    List[[st]] <- FUN(st)
  }
  List
}




