
testthat::context("Test Data2csv function")
library('MSEtool')
rm(list=ls())

dats<-avail('Data')
file <- "test.csv"
sim <- 1
for (dat in dats) {
  testthat::test_that(paste("Data2csv works with ", dat), {
    Data2csv(get(dat), file, simno = sim,overwrite=T)
    readDat <- new("Data", file)
    for (sl in slotNames('Data')) {
      if (!sl %in% c("TAC", "Sense", "MPrec", "PosMPs", "MPs")) {
        orig <- slot(get(dat), sl)
        if ("integer" %in% class(orig)) orig <- as.numeric(orig)
        read <- slot(readDat, sl)
        testthat::expect_equal(class(orig), class(read))
        if ('character' %in% class(orig)) {
          och <- nchar(orig)
          if (length(och)<1) och <- 0
          testthat::expect_equal(och, nchar(read))
        } else {
          if ('matrix' %in% class(orig) | 'array' %in% class(orig)) {
            if (length(dim(orig))==2) {
              nonna <- which(!is.na(orig[sim,]))
            } else {
              nonna <- which(!is.na(orig[sim,,]))
            }
                        if (length(nonna)>0) {
              samedims <- all(dim(orig) ==  dim(read))
              if (!grepl('CV_', sl))
                testthat::expect_true(samedims)
              if (length(dim(orig))>2) {
                # array
                testthat::expect_equal(orig[sim,nonna,], read[sim,nonna,])
              } else {
                testthat::expect_equal(orig[sim,nonna], read[sim,nonna])
              }
            }
          }
          if ("numeric" %in% class(orig)) testthat::expect_equal(orig[sim], read[sim])
        }
      }
    }
  })
}

file.remove(file)
