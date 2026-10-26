.LogUnusedRandom <- function(OM) {
  for (Stock in OM@Stock) {
    for (sl in c('Length', 'Weight', 'NaturalMortality')) {
      if (is.null(methods::slot(Stock, sl)@Random)) next
      Msg <- paste0("`", sl, "@Random` of stock '", Stock@Name, "' is ignored: ",
                    "`Random` multipliers are not used, so `", sl,
                    "@MeanAtAge` is applied without year- or simulation-specific variation.")
      if (Msg %in% vapply(OM@Log$assumption, \(e) e$message, character(1))) next
      OM <- .CaptureLog(OM, string = Msg, name = 'PopulateOM', type = 'assumption')
    }
  }
  OM
}
