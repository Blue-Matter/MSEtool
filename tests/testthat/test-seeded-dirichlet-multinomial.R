test_that(".SeededDirichletMultinomial is reproducible for a given key", {
  alpha <- rep(5, 20)
  key <- paste(1, 2030, 1, 1, "LandingsAtAge", 12, sep = "_")
  expect_identical(.SeededDirichletMultinomial(key, 500, alpha),
                   .SeededDirichletMultinomial(key, 500, alpha))
})

test_that(".SeededDirichletMultinomial gives distinct draws for permuted keys", {
  alpha <- rep(5, 20)
  key12 <- paste(1, 2030, 1, 1, "LandingsAtAge", 12, sep = "_")
  key21 <- paste(1, 2030, 1, 1, "LandingsAtAge", 21, sep = "_")
  expect_false(digest::digest2int(key12) == digest::digest2int(key21))
  expect_false(identical(.SeededDirichletMultinomial(key12, 500, alpha),
                         .SeededDirichletMultinomial(key21, 500, alpha)))
})

test_that(".SeededDirichletMultinomial seeds are near-unique across sims, years and fleets", {
  keys <- expand.grid(x = 1:192, yr = 2025:2054, fl = 1:3,
                      type = c("LandingsAtAge", "DiscardsAtAge"),
                      stringsAsFactors = FALSE)
  keys <- paste(1, keys$yr, 1, keys$fl, keys$type, keys$x, sep = "_")
  seeds <- vapply(keys, digest::digest2int, integer(1))
  expect_gt(length(unique(seeds)), 0.999 * length(keys))
})

test_that(".SeededDirichletMultinomial leaves the global RNG state untouched", {
  set.seed(42)
  before <- .Random.seed
  .SeededDirichletMultinomial("1_2030_1_1_LandingsAtAge_1", 100, rep(1, 5))
  expect_identical(.Random.seed, before)
})
