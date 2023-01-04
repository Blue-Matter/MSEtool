testthat::context("Data_Functions")

library(openMSE)


MSEextra()
library(MSEextra)
Dat <- avail('Data')
Dat <- Dat[!Dat %in% c("SimulatedData", "Simulation_1")]

for (dat in Dat) {
  testthat::test_that(paste("runMP, Can, Cant, Needed, Sense, TAC, and joinData work with ", dat), {
    datobj <- get(dat)
    cans <- Can(datobj)
    cants <- Cant(datobj)
    avails <- avail("MP")
    testthat::expect_error(out <<- runMP(datobj, chkMPs=T, silent=TRUE), NA)
    testthat::expect_true(length(out@MPs) == length(cans))
    testthat::expect_true(length(avails) - nrow(cants) == length(cans))
    testthat::expect_error(Needed(datobj), NA)
    # num <- which(MPtype(out@MPs)[,2]=='Output')
    # testthat::expect_error(Sense(out, out@MPs[num[1]]), NA)
    testthat::expect_error(TAC(datobj), NA)
    testthat::expect_error(joinData(list(datobj, datobj)), NA)
  })
}
