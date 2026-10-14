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
    expect_equal(unname(io@Efficiency[1]), 1, tolerance = 0.1)
    nom <- .CalcNomIndex(h@Number[1], h, 1, Names[a], io, HY, Units = "Number")
    expect_equal(as.numeric(nom), as.numeric(NAge[, as.character(Ages[a]), as.character(HY)]))
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

test_that("AddIndex adds a survey with its selectivity, or a fleet CPUE with the fleet's selectivity", {
  om <- .AgeIndexOM()
  HY <- Years(om, "H")
  Fleet <- FleetNames(om)[1]
  Index <- stats::setNames(seq_along(HY)[-(1:4)] / 10, HY[-(1:4)])

  om2 <- AddIndex(om, Index, Name = "Acoustic", Selectivity = "SBiomass", CV = 0.2, Timing = 0.5)
  Survey <- om2@Data[[1]]@Survey
  expect_equal(Survey@Name, "Acoustic")
  expect_equal(rownames(Survey@Value), as.character(HY))
  expect_true(all(is.na(Survey@Value[1:4, 1])))
  expect_equal(unname(Survey@Value[as.character(HY[-(1:4)]), 1]), unname(Index))
  expect_equal(Survey@Units, "Biomass")
  expect_equal(Survey@Timing, 0.5)
  expect_equal(om2@Obs[[1]][["Acoustic"]]@Survey@Selectivity, "SBiomass")

  # fleet CPUE: no Selectivity needed, and settings already on the fleet's Obs are kept
  om2@Obs[[1]][[Fleet]]@CPUE@Beta <- 0.8
  om3 <- AddIndex(om2, Index, Name = Fleet, Type = "CPUE")
  expect_equal(om3@Data[[1]]@CPUE@Name, Fleet)
  expect_null(om3@Obs[[1]][[Fleet]]@CPUE@Selectivity)
  expect_equal(om3@Obs[[1]][[Fleet]]@CPUE@Beta, 0.8)
  expect_equal(names(om3@Obs[[1]]), c(Fleet, "Acoustic"))

  # a single Data object in OM@Data
  om4 <- om
  om4@Data <- Data(Survey = IndicesData(Name = "Trawl", Value = matrix(1, length(HY), 1,
                                                                      dimnames = list(Year = HY, Fleet = "Trawl"))))
  om4 <- AddIndex(om4, Index, Name = "Acoustic", Selectivity = c("2" = 1, "3" = 1))
  expect_equal(om4@Data[[1]]@Survey@Name, c("Trawl", "Acoustic"))

  expect_error(AddIndex(om, Index, Name = "Acoustic"), "not a fleet")
  expect_error(AddIndex(om, Index, Name = tolower(Fleet), Type = "CPUE"), Fleet, fixed = TRUE)
  expect_error(AddIndex(om2, Index, Name = "Acoustic", Selectivity = "Biomass"), "already has")
  expect_error(AddIndex(om, Index, Name = "A", Selectivity = c("99" = 1)), "not in the stock's age classes")
  expect_error(AddIndex(om, Index, Name = "A", Selectivity = "Obs"), "Unknown")
  expect_error(AddIndex(om, Index, Name = "A", Selectivity = "Biomass",
                        IndexObs = IndicesObs(Selectivity = "Biomass")), "not both")
  expect_error(AddIndex(om, stats::setNames(1, 1900), Name = "A", Selectivity = "Biomass"), "outside")
})

test_that("an index added with AddIndex conditions and simulates", {
  skip_on_cran()
  om <- .AgeIndexOM()
  set.seed(1)
  h0 <- Simulate(om, silent = TRUE)
  HY <- Years(om, "H")
  SB <- SBiomass(h0, df = FALSE)[1, 1, ]
  Index <- SB / mean(SB) * exp(stats::rnorm(length(HY), 0, 0.1))
  Index[1:6] <- NA
  om2 <- AddIndex(om, stats::setNames(Index, HY), Name = "Acoustic", Selectivity = "SBiomass")
  set.seed(1)
  h <- Simulate(om2, silent = TRUE)
  io <- h@OM@Obs[[1]][["Acoustic"]]@Survey
  expect_length(io@Efficiency, 3)
  expect_lt(unname(io@CV[1]), 0.2)
})

test_that("IndexSelectivity gets and sets the selectivity of an index added through Data", {
  om <- .AgeIndexOM()
  HY <- Years(om, "H")
  Fleet <- FleetNames(om)[1]
  Value <- matrix(1, length(HY), 1, dimnames = list(Year = HY, Index = "Acoustic"))
  Data(om) <- Data(Survey = IndicesData(Name = "Acoustic", Value = Value))
  expect_null(IndexSelectivity(om, "Acoustic"))

  IndexSelectivity(om, "Acoustic") <- "SBiomass"
  expect_equal(IndexSelectivity(om, "Acoustic"), "SBiomass")
  expect_equal(names(om@Obs[[1]]), c(Fleet, "Acoustic"))

  om@Obs[[1]][["Acoustic"]]@Survey@Beta <- 0.7
  IndexSelectivity(om, "Acoustic") <- c("3" = 1, "4" = 1)
  expect_equal(IndexSelectivity(om, "Acoustic"), c("3" = 1, "4" = 1))
  expect_equal(om@Obs[[1]][["Acoustic"]]@Survey@Beta, 0.7)

  IndexSelectivity(om, Fleet, Type = "CPUE") <- "Biomass"
  expect_equal(IndexSelectivity(om, Fleet, Type = "CPUE"), "Biomass")
  IndexSelectivity(om, Fleet, Type = "CPUE") <- NULL
  expect_null(IndexSelectivity(om, Fleet, Type = "CPUE"))

  expect_error(IndexSelectivity(om, "Acoustic") <- NULL, "not a fleet")
  expect_error(IndexSelectivity(om, "Acoustic") <- c("99" = 1), "not in the stock's age classes")
})
