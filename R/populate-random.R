.LogDefaultR0 <- function(OM) {
  for (Stock in OM@Stock) {
    Default <- Stock@SRR@Misc$DefaultR0
    if (is.null(Default)) next
    Msg <- paste0("`SRR@R0` of stock '", Stock@Name, "' is not specified; using ", Default,
                  ". Absolute quantities (numbers, biomass, catch) are on this scale.")
    if (Msg %in% vapply(OM@Log$assumption, \(e) e$message, character(1))) next
    OM <- .CaptureLog(OM, string = Msg, name = 'PopulateOM', type = 'assumption')
  }
  OM
}

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
