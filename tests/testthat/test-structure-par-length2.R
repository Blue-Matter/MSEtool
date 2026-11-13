# A length-2 parameter is uniform bounds, except with nSim = 2 where it is one value per sim.

test_that("length-2 parameters are bounds unless nSim is 2, which gives a message", {
  rlang::local_options(rlib_message_verbosity = "verbose")
  bounds <- expect_no_message(.StructurePar(c(0.2, 0.3), nSim = 5, Years = 2000))
  expect_equal(dim(bounds)[1], 5)
  expect_true(all(bounds >= 0.2 & bounds <= 0.3))

  expect_message(perSim <- .StructurePar(c(0.2, 0.3), nSim = 2, Years = 2000), "one value per simulation")
  expect_equal(as.numeric(perSim), c(0.2, 0.3))

  expect_error(.StructurePar(c(0.2, 0.3), nSim = NULL), "nSim")
})

test_that("Depletion follows the same length-2 rule", {
  rlang::local_options(rlib_message_verbosity = "verbose")
  D <- Depletion(Initial = c(0.5, 0.3), Final = c(0.2, 0.4))
  bounds <- expect_no_message(PopulateDepletion(D, nSim = 5, seed = 1))
  expect_length(bounds@Initial, 5)
  expect_true(all(bounds@Initial >= 0.3 & bounds@Initial <= 0.5))

  expect_message(perSim <- PopulateDepletion(D, nSim = 2, seed = 1), "one value per simulation")
  expect_equal(as.numeric(perSim@Initial), c(0.5, 0.3))
  expect_equal(as.numeric(perSim@Final), c(0.2, 0.4))
})
