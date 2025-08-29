testthat::context("Test OM Plotting functions")

library(MSEtool)
library(testthat)

rm(list=ls())
# setup()

testthat::test_that("plot.Stock works with all available Stock objects", {
  rm(list=ls())
  objs <- avail('Stock')

  for (i in seq_along(objs)) {
    # graphics.off()
    obj <- get(objs[i])
    seed <- ceiling(runif(1, 1, 1000))
    set.seed(seed)
    info <- paste(objs[i], seed)
    testthat::expect_error(plot(obj, open=FALSE), NA, info=info)
    # expect_warning(plot(obj), NA, info=objs[i])
    # graphics.off()
  }
})

# cleanup
fls <- list.files(pattern = '.html')
unlink(fls)

testthat::test_that("plot.Fleet works with all available Fleet objects", {
  rm(list=ls())
  objs <- avail('Fleet')

  for (i in seq_along(objs)) {
    # graphics.off()
    obj <- get(objs[i])
    seed <- ceiling(runif(1, 1, 1000))
    set.seed(seed)
    stock <- sample(avail("Stock"),1)
    info <- paste(objs[i], stock, seed)
    testthat::expect_error(plot(obj, get(stock), open=FALSE), NA, info=info)
    # expect_warning(plotFleet(obj, get(stock)), NA, info=info)
    # graphics.off()
  }
})

# cleanup
fls <- list.files(pattern = '.html')
unlink(fls)


testthat::test_that("plot.Obs works with all available Obs objects", {
  rm(list=ls())
  objs <- avail('Obs')

  for (i in seq_along(objs)) {
    # graphics.off()
    obj <- get(objs[i])
    seed <- ceiling(runif(1, 1, 1000))
    set.seed(seed)
    info <- paste(objs[i], seed)
    testthat::expect_error(plot(obj, open=FALSE), NA, info=info)
    # expect_warning(plot(obj), NA, info=objs[i])
    # graphics.off()
  }
})

# cleanup
fls <- list.files(pattern = '.html')
unlink(fls)


testthat::test_that("plot.Imp works with all available Imp objects", {
  rm(list=ls())
  objs <- avail('Imp')

  for (i in seq_along(objs)) {
    # graphics.off()
    obj <- get(objs[i])
    seed <- ceiling(runif(1, 1, 1000))
    set.seed(seed)
    info <- paste(objs[i], seed)
    testthat::expect_error(plot(obj, open=FALSE), NA, info=info)
    # expect_warning(plot(obj), NA, info=objs[i])
    # graphics.off()
  }
})


# cleanup
fls <- list.files(pattern = '.html')
unlink(fls)


MSEextra()
library(MSEextra)
rm(list=ls())

testthat::test_that("plot.OM works with all OMs", {
  rm(list=ls())
  objs <- avail('OM')
  for (i in seq_along(objs)) {
    # graphics.off()
    obj <- get(objs[i])
    obj@nsim <- 48
    seed <- ceiling(runif(1, 1, 1000))
    obj@seed <- seed
    info <- paste(objs[i], seed)
    testthat::expect_error(plot(obj, silent=TRUE, open=FALSE), NA, info=info)
    # expect_warning(plot(obj), NA, info=objs[i])
    # graphics.off()
  }
})

# cleanup
fls <- list.files(pattern = '.html')
unlink(fls)

