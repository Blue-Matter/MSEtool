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
  expect_equal(Dim(out), c(Sim = 1L, Year = 4L))
  expect_equal(as.numeric(out), 1:4)
})

test_that("JoinYear falls back to object2 when object1 has zero Year rows", {
  # A zero-row Year-dimensioned array is vacuously all(is.na(.)) == TRUE in R;
  # JoinYear must not let that short-circuit and silently drop object2.
  empty <- array(numeric(0), dim = c(1, 0), dimnames = list(Sim = 1, Year = character(0)))
  full  <- array(1:3, dim = c(1, 3), dimnames = list(Sim = 1, Year = c("2001", "2002", "2003")))
  out <- JoinYear(empty, full)
  expect_equal(Dim(out), c(Sim = 1L, Year = 3L))
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
  expect_equal(Dim(out$x), c(Sim = 1L, Year = 2L))
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

test_that("JoinYear row-binds long-format data.frames with a Year column", {
  d1 <- expand.grid(Sim = 1:2, Year = 2001:2002, Fleet = c("A", "B"), stringsAsFactors = FALSE)
  d1$Value <- seq_len(nrow(d1))
  d2 <- expand.grid(Sim = 1:2, Year = 2003, Fleet = c("A", "B"), stringsAsFactors = FALSE)
  d2$Value <- 100 + seq_len(nrow(d2))
  out <- JoinYear(d1, d2[, rev(names(d2))])
  expect_equal(nrow(out), nrow(d1) + nrow(d2))
  expect_equal(names(out), names(d1))
  expect_equal(out$Value, c(d1$Value, d2$Value))
  expect_equal(rownames(out), as.character(seq_len(nrow(out))))
})

test_that("JoinYear aborts on non-increasing data.frame years", {
  d1 <- data.frame(Year = 2003:2004, Value = 1:2)
  d2 <- data.frame(Year = 2004:2005, Value = 3:4)
  expect_error(JoinYear(d1, d2), "not increasing")
})

test_that("JoinYear returns object2 for a zero-row data.frame object1", {
  d2 <- data.frame(Year = 2001:2002, Value = 1:2)
  expect_equal(JoinYear(d2[0, ], d2), d2)
})

test_that("JoinYear leaves identical or Year-less data.frames unchanged", {
  stats1 <- data.frame(Sim = 1:2, SD = c(0.2, 0.3))
  stats2 <- data.frame(Sim = 1:2, SD = c(0.4, 0.5))
  expect_equal(JoinYear(stats1, stats2), stats1)
  d <- data.frame(Year = 2001:2002, Value = 1:2)
  expect_equal(JoinYear(d, d), d)
})

test_that("JoinYear joins data.frames nested in lists", {
  a <- list(x = data.frame(Year = 2001, Fleet = "A", Value = 1))
  b <- list(x = data.frame(Year = 2002, Fleet = "A", Value = 2))
  out <- JoinYear(a, b)
  expect_equal(out$x$Year, c(2001, 2002))
  expect_equal(out$x$Fleet, c("A", "A"))
})

test_that("JoinYear compares character and factor Year columns numerically", {
  d1 <- data.frame(Year = factor(c("2009", "2010")), Value = 1:2)
  d2 <- data.frame(Year = factor("2011"), Value = 3)
  out <- JoinYear(d1, d2)
  expect_equal(as.character(out$Year), c("2009", "2010", "2011"))
  expect_error(JoinYear(data.frame(Year = "2010", Value = 1),
                        data.frame(Year = "9", Value = 2)), "not increasing")
})
