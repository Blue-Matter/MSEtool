CalcRecruitment_TimeStep <- function(OM, st=NULL) {
  CheckClass(OM, c('om', 'hist'))
  
  if (inherits(OM, 'hist')) 
    OM <- OM@OM
  
  if (!is.null(st)) {
    Stock <- OM@Stock[[st]]
    PreRecruit <- seq(0, by=1/Stock@Seasons, to=min(Stock@Ages@Classes))
    return(
      length(PreRecruit)-1
    )
  }
  
  purrr::map(OM@Stock, \(Stock) {
    PreRecruit <- seq(0, by=1/Stock@Seasons, to=min(Stock@Ages@Classes))
    length(PreRecruit)-1
  }) |> 
    List2Array('Stock') |>
    DropDimension('Sim')
  
}
