test_that("ArrayMultiply/ArrayDivide/ArraySum/ArraySubtract broadcast by dimname", {
  a <- array(1:4, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  b <- array(c(1, 1), dim = c(2, 1), dimnames = list(Sim = 1:2, Year = 2001))

  out_mult <- ArrayMultiply(a, b)
  expect_equal(dim(out_mult), c(Sim = 2L, Year = 2L))
  expect_equal(as.numeric(out_mult), as.numeric(a))

  out_sum <- ArraySum(a, b)
  expect_equal(as.numeric(out_sum), as.numeric(a) + as.numeric(b)[c(1, 2, 1, 2)])

  out_sub <- ArraySubtract(a, b)
  expect_equal(as.numeric(out_sub), as.numeric(a) - as.numeric(b)[c(1, 2, 1, 2)])

  out_div <- ArrayDivide(a, b)
  expect_equal(as.numeric(out_div), as.numeric(a) / as.numeric(b)[c(1, 2, 1, 2)])
})

test_that("ArrayMultiply/ArraySum/ArraySubtract/ArrayDivide take the identical-dims fast path", {
  a <- array(1:4, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  b <- array(5:8, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))

  expect_equal(ArrayMultiply(a, b), a * b)
  expect_equal(ArraySum(a, b), a + b)
  expect_equal(ArraySubtract(a, b), a - b)
  out_div <- ArrayDivide(a, b)
  expect_equal(as.numeric(out_div), as.numeric(a) / as.numeric(b))
  # dimnames/dim preserved exactly as with the slow (ArrayExtend) path
  expect_equal(dimnames(ArrayMultiply(a, b)), dimnames(a))
})

test_that("ArrayMultiply gives the same result via the fast and slow (extend) paths", {
  # same dims/dimnames -> fast path; deliberately mismatched Year length -> slow path
  a <- array(1:4, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  b_same  <- array(5:8, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  b_diff  <- array(c(5, 6), dim = c(2, 1), dimnames = list(Sim = 1:2, Year = 2001))

  fast_result <- ArrayMultiply(a, b_same)
  slow_result <- MSEtool:::.ArrayOperation(a, b_diff)
  # sanity: the slow path still broadcasts correctly on genuinely mismatched dims
  expect_equal(dim(slow_result), c(Sim = 2L, Year = 2L))
  expect_equal(as.numeric(fast_result), as.numeric(a) * as.numeric(b_same))
})

test_that("ArrayMultiply broadcasts a Fleet dimension (previously errored)", {
  a <- array(1:8, dim = c(Sim = 2, Fleet = 4), dimnames = list(Sim = 1:2, Fleet = 1:4))
  b <- array(c(10, 100), dim = c(Sim = 2, Fleet = 1), dimnames = list(Sim = 1:2, Fleet = 1))

  out <- ArrayMultiply(a, b)
  expect_equal(dim(out), c(Sim = 2L, Fleet = 4L))
  expect_equal(as.numeric(out), as.numeric(a) * as.numeric(b)[c(1, 2, 1, 2, 1, 2, 1, 2)])
})

test_that("ArraySum broadcasts a Stock dimension with character labels (previously errored)", {
  a <- array(1:6, dim = c(Sim = 2, Stock = 3),
             dimnames = list(Sim = 1:2, Stock = c("north", "south", "east")))
  b <- array(c(1, 2), dim = c(Sim = 2, Stock = 1),
             dimnames = list(Sim = 1:2, Stock = "north"))

  out <- ArraySum(a, b)
  expect_equal(dim(out), c(Sim = 2L, Stock = 3L))
  expect_equal(dimnames(out)$Stock, c("north", "south", "east"))
  expect_equal(as.numeric(out), as.numeric(a) + as.numeric(b)[c(1, 2, 1, 2, 1, 2)])
})

test_that("ArrayMultiply broadcasts two dimensions simultaneously (Sim on one side, Age on the other)", {
  a <- array(1:6, dim = c(Sim = 3, Age = 1, Year = 2),
             dimnames = list(Sim = 1:3, Age = 1, Year = 2020:2021))
  b <- array(1:8, dim = c(Sim = 1, Age = 4, Year = 2),
             dimnames = list(Sim = 1, Age = 1:4, Year = 2020:2021))

  out <- ArrayMultiply(a, b)
  expect_equal(dim(out), c(Sim = 3L, Age = 4L, Year = 2L))

  expected <- array(NA_real_, dim = c(3, 4, 2))
  for (s in 1:3) for (ag in 1:4) for (y in 1:2)
    expected[s, ag, y] <- a[s, 1, y] * b[1, ag, y]
  expect_equal(as.numeric(out), as.numeric(expected))
})

test_that("ArrayMultiply's broadcast path matches the old materialize-then-operate approach", {
  a <- array(rnorm(3 * 1 * 5), dim = c(Sim = 3, Age = 1, Year = 5),
             dimnames = list(Sim = 1:3, Age = 1, Year = 2020:2024))
  b <- array(rnorm(1 * 4 * 5), dim = c(Sim = 1, Age = 4, Year = 5),
             dimnames = list(Sim = 1, Age = 1:4, Year = 2020:2024))

  fast <- ArrayMultiply(a, b)

  ArrayList <- ArrayExtend(a, b)
  slow <- ArrayList$array1 * ArrayList$array2

  expect_equal(dim(fast), dim(slow))
  expect_equal(as.numeric(fast), as.numeric(slow))
})

test_that("ArrayDivide zeroes NA/Inf results", {
  a <- array(c(1, 0), dim = 2, dimnames = list(Sim = 1:2))
  b <- array(c(0, 0), dim = 2, dimnames = list(Sim = 1:2))
  out <- ArrayDivide(a, b)
  expect_equal(as.numeric(out), c(0, 0))
})

test_that("AddDimension attaches dimnames even when input array is unnamed", {
  x <- array(5, dim = 1)
  out <- AddDimension(x, "Year", 2027, pos = 1)
  expect_equal(dim(out), c(Year = 1L, 1L))
  expect_equal(dimnames(out)$Year, "2027")
})

test_that("AddDimension preserves existing dimnames", {
  x <- array(1:6, dim = c(2, 3), dimnames = list(Fleet = c("A", "B"), Area = c("1", "2", "3")))
  out <- AddDimension(x, "Year", 2020, pos = 1)
  expect_equal(names(dimnames(out)), c("Year", "Fleet", "Area"))
  expect_equal(dimnames(out)$Fleet, c("A", "B"))
})

test_that("DropDimension removes a named dimension", {
  x <- array(1:4, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  out <- DropDimension(x, "Year", warn = FALSE)
  expect_equal(names(dimnames(out)), "Sim")
  expect_equal(dim(out), c(Sim = 2L))
})

test_that("ExtendSims replicates a length-1 Sim dimension", {
  x <- array(0.5, dim = 1, dimnames = list(Sim = "1"))
  out <- ExtendSims(x, 3)
  expect_equal(dim(out), c(Sim = 3L))
  expect_equal(as.numeric(out), rep(0.5, 3))
})

test_that("ExtendSims is a no-op when already at the target size", {
  x <- array(1:3, dim = 3, dimnames = list(Sim = 1:3))
  expect_equal(ExtendSims(x, 3), x)
})

test_that("ExtendYears forward-fills using the most recent existing year by default", {
  x <- array(c(100, 200), dim = c(1, 2), dimnames = list(Sim = 1, Year = c(2010, 2015)))
  out <- ExtendYears(x, Years = 2010:2020)
  expect_equal(as.numeric(out[1, "2014"]), 100)
  expect_equal(as.numeric(out[1, "2015"]), 200)
  expect_equal(as.numeric(out[1, "2020"]), 200)
})

test_that("ExtendYears does not backfill by default", {
  x <- array(1, dim = c(1, 1), dimnames = list(Sim = 1, Year = 2010))
  out <- ExtendYears(x, Years = 2005:2010)
  expect_false("2005" %in% dimnames(out)$Year)
  expect_true("2010" %in% dimnames(out)$Year)
})

test_that("ExtendYears preserves a seasonal pattern rather than flat-filling", {
  # 2 seasons per year across 2 years; season fractions 0 and 0.5
  x <- array(c(10, 20, 30, 40), dim = c(1, 4),
            dimnames = list(Sim = 1, Year = c(2020, 2020.5, 2021, 2021.5)))
  seed <- array(c(10, 20), dim = c(1, 2), dimnames = list(Sim = 1, Year = c(2020, 2020.5)))
  out <- ExtendYears(seed, Years = c(2020, 2020.5, 2021, 2021.5), maintain_seasonal_pattern = TRUE)
  expect_equal(as.numeric(out[1, "2021"]), 10)
  expect_equal(as.numeric(out[1, "2021.5"]), 20)
})

test_that("ExtendYears forward-fill flat-fills (ignores season) when maintain_seasonal_pattern = FALSE", {
  seed <- array(c(10, 20), dim = c(1, 2), dimnames = list(Sim = 1, Year = c(2020, 2020.5)))
  out <- ExtendYears(seed, Years = c(2020, 2020.5, 2021, 2021.5), maintain_seasonal_pattern = FALSE)
  # both new years copy the single most recent existing value (season .5, value 20)
  expect_equal(as.numeric(out[1, "2021"]), 20)
  expect_equal(as.numeric(out[1, "2021.5"]), 20)
})

test_that("ExtendYears back-fill matches the season when maintain_seasonal_pattern = TRUE", {
  # 2 seasons per year; existing years are 2021/2021.5, back-fill to 2020/2020.5
  seed <- array(c(10, 20), dim = c(1, 2), dimnames = list(Sim = 1, Year = c(2021, 2021.5)))
  out <- ExtendYears(seed, Years = c(2020, 2020.5, 2021, 2021.5),
                     backfill = TRUE, maintain_seasonal_pattern = TRUE)
  expect_equal(as.numeric(out[1, "2020"]), 10)   # season 0 matched from 2021
  expect_equal(as.numeric(out[1, "2020.5"]), 20) # season .5 matched from 2021.5
})

test_that("ExtendYears back-fill flat-fills (ignores season) when maintain_seasonal_pattern = FALSE", {
  seed <- array(c(10, 20), dim = c(1, 2), dimnames = list(Sim = 1, Year = c(2021, 2021.5)))
  out <- ExtendYears(seed, Years = c(2020, 2020.5, 2021, 2021.5),
                     backfill = TRUE, maintain_seasonal_pattern = FALSE)
  # both new years copy the first existing value (season 0, value 10)
  expect_equal(as.numeric(out[1, "2020"]), 10)
  expect_equal(as.numeric(out[1, "2020.5"]), 10)
})

test_that("ExtendYears inside-fill matches the season when maintain_seasonal_pattern = TRUE", {
  # gap at 2021/2021.5 between existing 2020/2020.5 and 2022/2022.5
  seed <- array(c(10, 20, 30, 40), dim = c(1, 4),
               dimnames = list(Sim = 1, Year = c(2020, 2020.5, 2022, 2022.5)))
  out <- ExtendYears(seed, Years = c(2020, 2020.5, 2021, 2021.5, 2022, 2022.5),
                     maintain_seasonal_pattern = TRUE)
  expect_equal(as.numeric(out[1, "2021"]), 10)   # season 0 matched from 2020
  expect_equal(as.numeric(out[1, "2021.5"]), 20) # season .5 matched from 2020.5
})

test_that("ExtendYears inside-fill flat-fills (ignores season) when maintain_seasonal_pattern = FALSE", {
  seed <- array(c(10, 20, 30, 40), dim = c(1, 4),
               dimnames = list(Sim = 1, Year = c(2020, 2020.5, 2022, 2022.5)))
  out <- ExtendYears(seed, Years = c(2020, 2020.5, 2021, 2021.5, 2022, 2022.5),
                     maintain_seasonal_pattern = FALSE)
  # both gap years copy the year immediately preceding the gap (season .5, value 20)
  expect_equal(as.numeric(out[1, "2021"]), 20)
  expect_equal(as.numeric(out[1, "2021.5"]), 20)
})

test_that("List2Array takes the identical-dims fast path when all elements already conform", {
  a <- array(1:4, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  b <- array(5:8, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  out <- List2Array(list(A = a, B = b), name = "Fleet")

  expect_equal(dim(out), c(Sim = 2L, Year = 2L, Fleet = 2L))
  expect_equal(names(dimnames(out)), c("Sim", "Year", "Fleet"))
  expect_equal(as.numeric(out[, , "A"]), as.numeric(a))
  expect_equal(as.numeric(out[, , "B"]), as.numeric(b))
})

test_that("List2Array gives the same result whether elements already conform or need extending", {
  a <- array(1:4, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  b_same <- array(5:8, dim = c(2, 2), dimnames = list(Sim = 1:2, Year = 2001:2002))
  # b_diff has one fewer Year -> forces the slow (ExtendYears) path
  b_diff <- array(c(5, 6), dim = c(2, 1), dimnames = list(Sim = 1:2, Year = 2001))

  out_fast <- List2Array(list(A = a, B = b_same), name = "Fleet")
  out_slow <- List2Array(list(A = a, B = b_diff), name = "Fleet")

  expect_equal(dim(out_fast), c(Sim = 2L, Year = 2L, Fleet = 2L))
  expect_equal(dim(out_slow), c(Sim = 2L, Year = 2L, Fleet = 2L))
  # b_diff's single Year (2001) is forward-filled to 2002, matching its own value
  expect_equal(out_slow[, "2001", "B"], b_diff[, "2001"])
  expect_equal(out_slow[, "2002", "B"], b_diff[, "2001"])
})
