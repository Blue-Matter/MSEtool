
AddSimNumber <- function(Hist) {
  DataSimList <- Hist@Data
  DataSimList <- purrr::imap(DataSimList, \(DataStock, i)
              purrr::map(DataStock, \(Data) {
                Data@Misc$Sim  <- as.numeric(i)
                Data  
              })
  )
  Hist@Data <- DataSimList
  Hist
}