test_that("JoinYear concatenates numeric year vectors", {
  expect_equal(JoinYear(c(2001, 2002), c(2003, 2004)), c(2001, 2002, 2003, 2004))
})

test_that("JoinYear aborts on non-increasing numeric years", {
  expect_error(JoinYear(c(2003, 2004), c(2001, 2002)))
})

test_that("JoinYear joins arrays along the Year dimension", {
  a <- array(1:2, dim = c(1, 2), dimnames = list(Sim = 1, Year = c("2001", "2002")))
  b <- array(3:4, dim = c(1, 2), dimnames = list(Sim = 1, Year = c("2003", "2004")))
  out <- JoinYear(a, b)
  expect_equal(dim(out), c(Sim = 1L, Year = 4L))
  expect_equal(as.numeric(out), 1:4)
})

test_that("JoinYear falls back to object2 when object1 has zero Year rows", {
  # A zero-row Year-dimensioned array is vacuously all(is.na(.)) == TRUE in R;
  # JoinYear must not let that short-circuit and silently drop object2.
  empty <- array(numeric(0), dim = c(1, 0), dimnames = list(Sim = 1, Year = character(0)))
  full  <- array(1:3, dim = c(1, 3), dimnames = list(Sim = 1, Year = c("2001", "2002", "2003")))
  out <- JoinYear(empty, full)
  expect_equal(dim(out), c(Sim = 1L, Year = 3L))
  expect_equal(as.numeric(out), 1:3)
})

test_that("JoinYear falls back to object2 when object1's other dims mismatch object2's", {
  # object1 (e.g. a stitched-in historical placeholder) may have a
  # differently-shaped non-Year dimension than object2 (e.g. one MP issuing
  # per-fleet TAC vs another issuing a single pooled TAC) -- since object1
  # contributes zero Year rows, its shape is irrelevant and should not
  # block the join.
  empty_2fleet <- array(numeric(0), dim = c(1, 2, 0),
                        dimnames = list(Sim = 1, Fleet = c("A", "B"), Year = character(0)))
  full_1fleet <- array(1:3, dim = c(1, 1, 3),
                       dimnames = list(Sim = 1, Fleet = "Total", Year = c("2001", "2002", "2003")))
  out <- JoinYear(empty_2fleet, full_1fleet)
  expect_equal(as.numeric(out), 1:3)
})

test_that("JoinYear recurses through S4 slots and falls back to val2 when val1 is NULL", {
  hist_advice <- methods::new("advicedata")
  proj_advice <- methods::new("advicedata")
  proj_advice@TAC <- array(1:3, dim = c(1, 3), dimnames = list(Sim = 1, Year = c("2001", "2002", "2003")))

  out <- JoinYear(hist_advice, proj_advice)
  expect_equal(as.numeric(out@TAC), 1:3)
})

test_that("JoinYear recurses through lists element-wise", {
  a <- list(x = array(1, dim = c(1, 1), dimnames = list(Sim = 1, Year = "2001")))
  b <- list(x = array(2, dim = c(1, 1), dimnames = list(Sim = 1, Year = "2002")))
  out <- JoinYear(a, b)
  expect_equal(dim(out$x), c(Sim = 1L, Year = 2L))
})

test_that("JoinYear does not duplicate identical non-year numeric vectors", {
  # e.g. a compdata's per-fleet `Classes` bin boundaries, which are static
  # metadata shared by the historical and projection segments of a `data`
  # object and must not be concatenated just because length(object1) > 1.
  bins <- seq(10, 160, by = 2)
  out <- JoinYear(bins, bins)
  expect_equal(out, bins)
})

test_that("JoinYear still concatenates differing non-year numeric vectors", {
  out <- JoinYear(c(1, 2, 3), c(4, 5))
  expect_equal(out, c(1, 2, 3, 4, 5))
})
