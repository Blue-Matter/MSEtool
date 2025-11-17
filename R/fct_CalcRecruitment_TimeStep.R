CalcRecruitment_TimeStep <- function(OM, st=NULL) {
  CheckClass(OM, c('om', 'hist'))
  
  if (inherits(OM, 'hist')) 
    OM <- OM@OM
  
  if (!is.null(st)) {
    Stock <- OM@Stock[[st]]
    FullAgeClasses <- seq(0, by=1/Stock@TSperYear, to=max(Stock@Ages@Classes))
    return(match(min(Stock@Ages@Classes), FullAgeClasses))
  }
  
  purrr::map(OM@Stock, \(Stock) {
    FullAgeClasses <- seq(0, by=1/Stock@TSperYear, to=max(Stock@Ages@Classes))
    match(min(Stock@Ages@Classes), FullAgeClasses)
  }) |> 
    List2Array('Stock') |>
    DropDimension('Sim')
  
}
