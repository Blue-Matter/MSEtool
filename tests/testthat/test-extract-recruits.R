skip_on_cran()

.RecruitsFromNumber <- function(object) {
  Number(object, byAge = TRUE) |>
    dplyr::group_by(.data$Stock) |>
    dplyr::filter(.data$Age == min(.data$Age)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Stock = as.character(.data$Stock)) |>
    dplyr::select(dplyr::any_of(c('Sim', 'Stock', 'Year', 'MP')), Expected = 'Value')
}

.JoinRecruits <- function(rec, expected) {
  rec |>
    dplyr::mutate(Stock = as.character(.data$Stock)) |>
    dplyr::inner_join(expected, by = intersect(c('Sim', 'Stock', 'Year', 'MP'), names(expected)))
}

data(MultiStockOM, envir = environment())
om <- MultiStockOM
om@nSim <- 3
om@pYear <- 4
set.seed(101)
hist <- Simulate(om, silent = TRUE)
mse  <- Project(hist, MPs = c('CurrentEffort', 'FMSYref'), parallel = FALSE, silent = TRUE)

test_that("Recruits() equals numbers in the youngest age class (hist)", {
  rec <- Recruits(hist, Extend = TRUE)
  expect_s3_class(rec, 'recruits.df')
  expect_true(all(rec$Period == 'Historical'))
  expect_true(all(rec$Variable == 'Recruits'))
  expect_setequal(unique(rec$Year), Years(hist, 'Historical'))
  expect_equal(nrow(rec), om@nSim * nStock(om) * length(Years(hist, 'Historical')))

  joined <- .JoinRecruits(rec, .RecruitsFromNumber(hist))
  expect_equal(nrow(joined), nrow(rec))
  expect_equal(joined$Value, joined$Expected)
  expect_true(any(joined$Value > 0))
  expect_gt(length(unique(rec$Value[rec$Year == max(rec$Year)])), 1)
})

test_that("Recruits() covers both periods and all MPs (mse)", {
  rec <- Recruits(mse, Extend = TRUE)
  expect_setequal(unique(rec$MP), c('Historical', 'CurrentEffort', 'FMSYref'))
  expect_setequal(unique(rec$Year[rec$Period == 'Projection']), Years(mse, 'Projection'))
  expect_setequal(unique(rec$Year[rec$Period == 'Historical']), Years(mse, 'Historical'))

  joined <- .JoinRecruits(rec, .RecruitsFromNumber(mse))
  expect_equal(nrow(joined), nrow(rec))
  expect_equal(joined$Value, joined$Expected)

  arr <- Recruits(mse, df = FALSE)
  expect_equal(names(dimnames(arr)), c('Sim', 'Stock', 'Year', 'MP'))
  expect_equal(dimnames(arr)$MP, c('CurrentEffort', 'FMSYref'))
  expect_equal(dimnames(arr)$Year, as.character(Years(mse, 'Projection')))
})

test_that("Recruits(Units = 'Biomass') multiplies by weight of the youngest age class", {
  num <- Recruits(hist, df = FALSE)
  bio <- Recruits(hist, df = FALSE, Units = 'Biomass')
  hYears <- Years(hist, 'Historical')
  for (st in seq_len(nStock(om))) {
    w <- hist@OM@Stock[[st]]@Weight@MeanAtAge |> ExtendSims(om@nSim) |>
      ExtendYears(Years = hYears) |> .SubsetYear(hYears)
    w1 <- matrix(w[, 1, ], om@nSim, length(hYears))
    n1 <- matrix(ExtendSims(num, om@nSim)[, st, ], om@nSim, length(hYears))
    expect_equal(as.numeric(ExtendSims(bio, om@nSim)[, st, ]), as.numeric(n1 * w1))
  }
  df <- Recruits(hist, Units = 'Biomass')
  expect_false(any(is.na(df$Units)))
})

test_that("Recruits(byArea = TRUE) sums to the area total", {
  byArea <- Recruits(hist, df = FALSE, byArea = TRUE)
  total  <- Recruits(hist, df = FALSE)
  expect_true('Area' %in% names(dimnames(byArea)))
  summed <- SumOverArea(byArea)
  expect_equal(dimnames(summed), dimnames(total))
  expect_equal(as.numeric(summed), as.numeric(total))
})

test_that("Seasonal Recruits() follows the recruitment pulse and Annual sums calendar years", {
  data(SeasonalSpatialOM, envir = environment())
  sOM <- SeasonalSpatialOM
  sOM@nSim <- 3
  set.seed(2)
  sHist <- Simulate(sOM, silent = TRUE)

  ts <- Recruits(sHist, df = FALSE) |> ExtendSims(3)
  expect_equal(unname(dim(ts)[3]), sOM@nYear * sOM@Seasons)
  joined <- .JoinRecruits(Recruits(sHist, Extend = TRUE), .RecruitsFromNumber(sHist))
  expect_equal(joined$Value, joined$Expected)
  firstYear <- ts[1, 1, 1:12]
  expect_gt(max(firstYear) / min(firstYear), 100)

  ann <- Recruits(sHist, df = FALSE, Annual = TRUE) |> ExtendSims(3)
  cal <- floor(as.numeric(dimnames(ts)$Year) + 1e-8)
  expect_equal(dimnames(ann)$Year, as.character(unique(cal)))
  for (y in dimnames(ann)$Year)
    expect_equal(as.numeric(ann[, , y]),
                 as.numeric(apply(ts[, , cal == as.numeric(y), drop = FALSE], 1:2, sum)))

  df <- Recruits(sHist, Annual = TRUE)
  expect_true(all(df$Year == floor(df$Year)))
})
