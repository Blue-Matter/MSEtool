.CheckRelations <- function(OM) {
  if (!length(OM@Relations))
    return(OM)
  Msg <- paste0("`OM@Relations` is ignored: relationships among stocks are not modelled, ",
                "so each stock is simulated with its own dynamics (",
                length(OM@Relations), " relationship(s) supplied).")
  if (Msg %in% vapply(OM@Log$warning, \(e) e$message, character(1)))
    return(OM)
  .CaptureLog(OM, string = Msg, name = 'Simulate', type = 'warning')
}
