.AgeIndexOM <- function(nSim = 3) {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  nSim(om) <- nSim
  om
}

.NumberAtAge <- function(hist) apply(hist@Number[[1]], c(1, 2, 3), sum)

test_that("AddAgeIndices adds one knife-edge index per age that conditions and projects", {
  skip_on_cran()
  om <- .AgeIndexOM()
  set.seed(1)
  h0 <- Simulate(om, silent = TRUE)
  HY <- Years(om, "H")
  Ages <- c(2, 3, 5)
  NAge <- .NumberAtAge(h0)

  Value <- t(NAge[1, as.character(Ages), as.character(HY)]) *
    exp(matrix(stats::rnorm(length(HY) * length(Ages), 0, 0.1), length(HY)))
  dimnames(Value) <- list(Year = HY, Age = Ages)
  Value[1:5, ] <- NA
  Value[8, 1] <- Value[12, 3] <- NA
  # rows need not cover every historical year
  Value <- Value[-c(1:3), ]

  om2 <- AddAgeIndices(om, Value, Name = "Acoustic", CV = 0.2)
  Names <- paste("Acoustic Age", Ages)
  Survey <- om2@Data[[1]]@Survey
  expect_equal(Survey@Name, Names)
  expect_equal(rownames(Survey@Value), as.character(HY))
  expect_true(all(is.na(Survey@Value[1:5, ])))
  expect_equal(unname(Survey@Value[as.character(HY[-c(1:3)]), ]), unname(Value))
  expect_equal(unname(Survey@CV[!is.na(Survey@Value)]), rep(0.2, sum(!is.na(Value))))
  expect_equal(Survey@Units, rep("Number", 3))
  expect_equal(names(om2@Obs[[1]]), c(FleetNames(om), Names))
  expect_equal(om2@Obs[[1]][["Acoustic Age 3"]]@Survey@Selectivity, c("3" = 1))

  set.seed(1)
  h <- Simulate(om2, silent = TRUE)
  for (a in seq_along(Ages)) {
    io <- h@OM@Obs[[1]][[Names[a]]]@Survey
    expect_length(io@Efficiency, 3)
    expect_equal(io@Efficiency[1], 1, tolerance = 0.1)
    nom <- .CalcNomIndex(h@Number[1], h, 1, Names[a], io, HY, Units = "Number")
    expect_equal(unname(nom), unname(NAge[, as.character(Ages[a]), as.character(HY)]))
    nom2 <- .CalcNomIndex(h@Number[1], h, 1, Names[a], io, HY, Units = "Number", sim = 2)
    expect_equal(as.numeric(nom2), as.numeric(nom[2, ]))
  }

  mse <- Project(h, MPs = "NFref", parallel = FALSE, silent = TRUE)
  PY <- as.character(Years(om, "P"))
  for (x in 1:3) {
    v <- mse@PPD[[1]][[x]][[1]]@Survey@Value[, Names]
    v <- v[intersect(PY, rownames(v)), ]
    expect_true(all(is.finite(v) & v > 0))
  }
})

test_that("AddAgeIndices appends to existing indices and validates its inputs", {
  om <- .AgeIndexOM()
  HY <- Years(om, "H")
  Existing <- matrix(seq_along(HY), ncol = 1, dimnames = list(Year = HY, Fleet = "Trawl"))
  Data(om) <- Data(Name = "x", Years = HY,
                   Survey = IndicesData(Name = "Trawl", Value = Existing, Units = "Biomass", Timing = 0.5))
  om@Obs[[1]][["Trawl"]] <- Obs("Trawl", Survey = IndicesObs(Selectivity = "Biomass"))

  Value <- matrix(1, length(HY), 2, dimnames = list(Year = HY, Age = c(1, 4)))
  om2 <- AddAgeIndices(om, Value, Name = "Acoustic", Timing = 0.25)
  Survey <- om2@Data[[1]]@Survey
  expect_equal(Survey@Name, c("Trawl", "Acoustic Age 1", "Acoustic Age 4"))
  expect_equal(unname(Survey@Value[, "Trawl"]), as.numeric(seq_along(HY)))
  expect_equal(Survey@Units, c("Biomass", "Number", "Number"))
  expect_equal(Survey@Timing, c(0.5, 0.25, 0.25))
  expect_null(Survey@CV)
  expect_equal(names(om2@Obs[[1]]), c(FleetNames(om), "Trawl", "Acoustic Age 1", "Acoustic Age 4"))

  expect_error(AddAgeIndices(om2, Value, Name = "Acoustic"), "already used")
  bad <- Value; colnames(bad) <- c(1, 99)
  expect_error(AddAgeIndices(om, bad), "not in the age classes")
  bad <- Value; rownames(bad)[1] <- 1900
  expect_error(AddAgeIndices(om, bad), "outside")
  expect_error(AddAgeIndices(om, unname(Value)), "row names")
  expect_error(AddAgeIndices(om, Value, IndexObs = IndicesObs(Selectivity = "Biomass")), "must be")
  expect_error(AddAgeIndices(om, Value, CV = c(0.1, 0.2)), "CV")
})
