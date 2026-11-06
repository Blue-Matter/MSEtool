# data of the first layer of a plot with all of Cols
.LayerData <- function(p, Cols) {
  for (L in p$layers)
    if (is.data.frame(L$data) && all(Cols %in% names(L$data))) return(L$data)
  NULL
}

test_that("ReplayMP() reproduces the recorded advice and builds the animations", {
  skip_on_cran()
  skip_if_not_installed('gganimate')
  HoldC <- function(Data) Advice(TAC = 0.9 * LastTAC(Data))
  class(HoldC) <- 'mp'
  Hist <- Simulate(SingleStockOM, nSim = 2, silent = TRUE)
  MSE  <- Project(Hist, list(IndexRate = IndexRate, SurplusProduction = SurplusProduction,
                             HoldC = HoldC,
                             SPLate = SetMPArgs(SurplusProduction, MinIndexYears = 23),
                             IR3 = structure(IndexRate, Interval = 3)), silent = TRUE)
  Proj <- Years(MSE@OM, 'Projection')

  R <- expect_no_warning(ReplayMP(MSE, 'IndexRate', Sim = 2))
  expect_s3_class(R, 'mpreplay')
  expect_equal(R$Table$Year, Proj)
  expect_equal(R$Table$Advice, R$Table$Recorded, tolerance = 1e-8)
  expect_named(R$Animations, c('Index', 'Status', 'HCR', 'Catch'))
  for (a in R$Animations) expect_s3_class(a, 'gganim')
  for (a in R$Animations[c('Index', 'Status', 'Catch')])
    expect_true(any(vapply(a$layers, \(L) inherits(L$geom, 'GeomVline'), logical(1))))
  Lines <- .LayerData(R$Animations$Status, c('Year', 'Value', 'Series', 'Frame'))
  expect_gt(nrow(Lines), 0)
  expect_true(all(as.numeric(Lines$Year) <= as.numeric(as.character(Lines$Frame))))
  OMS <- MSEtool:::.ReplayOMStatus(MSE, 'IndexRate', 2, R$Stock, 'SB')
  K <- attr(OMS, 'K')
  expect_true(K > 0 && K < 1)
  MSY <- MSEtool:::.ReplayOMRelative(MSE, 'IndexRate', 2, R$Stock, 'SB_SBMSY')
  Both <- merge(OMS, MSY, by = 'Year')
  expect_equal(Both$Value.x, Both$Value.y * K, tolerance = 0.05)
  OMI <- .LayerData(R$Animations$Index, c('Name', 'Year', 'Value', 'Frame'))
  expect_gt(nrow(OMI), 0)
  expect_true(all(OMI$Year < as.numeric(as.character(OMI$Frame))))
  CatchSeries <- unique(as.character(.LayerData(R$Animations$Catch, c('Series', 'Value'))$Series))
  expect_true(all(c('Landings', 'Past TACs') %in% CatchSeries))
  expect_named(ReplayMP(MSE, 'IndexRate', Sim = 2, Years = Proj[1:2], Status = NULL)$Animations,
               c('Index', 'HCR', 'Catch'))
  expect_error(ReplayMP(MSE, 1, Status = 'SB_SBMSY'), 'Status')

  Figs <- ReplayPlot(R, Proj[3])
  expect_named(Figs, c('Index', 'Status', 'HCR', 'Catch'))
  for (f in Figs) {
    expect_s3_class(f, 'ggplot')
    expect_false(inherits(f, 'gganim'))
  }
  expect_match(Figs$HCR$labels$title, as.character(Proj[3]))
  Pts <- Figs$HCR$layers[[length(Figs$HCR$layers) - 1]]$data
  expect_equal(unique(as.character(Pts$Frame)), as.character(Proj[3]))
  expect_length(unique(R$Animations$HCR$layers[[4]]$data$Frame), length(Proj))
  expect_s3_class(ReplayPlot(R, Proj[3], 'Catch'), 'ggplot')
  Full <- ReplayMP(MSE, 'IndexRate', Sim = 2, Years = Proj[1:2], IndexCap = Inf)$Animations$Index
  Cut  <- ReplayMP(MSE, 'IndexRate', Sim = 2, Years = Proj[1:2], IndexCap = 0.5)$Animations$Index
  First <- \(a) min(.LayerData(a, 'Observed')$Year)
  expect_gt(First(Cut), First(Full))
  expect_equal(min(diff(sort(unique(.LayerData(Cut, 'Observed')$Year)))), 1)
  expect_error(ReplayMP(MSE, 1, IndexCap = 0), 'IndexCap')
  expect_error(ReplayPlot(R, 1900), 'management years')

  skip_if_not_installed('gifski')
  Short <- ReplayMP(MSE, 'IndexRate', Sim = 2, Years = Proj[1:3])
  File  <- tempfile(fileext = '.gif')
  G <- ReplayAnimate(Short, Plot = c('HCR', 'Catch'), fps = 4, width = 500, height = 250, file = File)
  expect_s3_class(G, 'gif_image')
  expect_true(file.exists(File))
  expect_error(ReplayAnimate(Short, Plot = 'Biomass'), 'No animation')
  expect_error(ReplayPlot(R, Proj[3], 'Biomass'), 'No animation')

  R <- expect_no_warning(ReplayMP(MSE, 2, Years = Proj[c(1, 5)]))
  expect_equal(R$Table$Year, Proj[c(1, 5)])
  expect_equal(R$Table$Advice, R$Table$Recorded, tolerance = 1e-6)
  expect_named(R$Animations, c('Index', 'Status', 'HCR', 'Catch'))

  expect_message(R <- ReplayMP(MSE, 'HoldC', Years = Proj[1:3]), 'only the')
  expect_named(R$Animations, c('Status', 'Catch'))
  expect_equal(R$Table$Advice, R$Table$Recorded)

  R <- expect_no_warning(ReplayMP(MSE, 'SPLate', Years = Proj[1:6]))
  expect_equal(R$Table$Fallback, Proj[1:6] < Proj[1] + 3)
  expect_equal(R$Table$Advice, R$Table$Recorded, tolerance = 1e-6)
  Now <- Filter(\(L) 'What' %in% names(L$data), R$Animations$Catch$layers)[[1]]$data
  Flagged <- unique(as.numeric(as.character(Now$Frame[Now$What == 'TAC (fit not used)'])))
  expect_equal(Flagged, Proj[1:3])
  expect_no_warning(for (y in Proj[c(1, 6)]) ggplot2::ggplot_build(ReplayPlot(R, y, 'Status')))

  R <- expect_no_warning(ReplayMP(MSE, 'IR3', Sim = 2))
  expect_equal(R$Table$Year, Proj[seq(1, length(Proj), by = 3)])
  Recorded <- PPD(MSE)[['IR3']][[2]][[1]]@Advice@TAC[, 1]
  Trail <- Filter(\(L) 'Series' %in% names(L$data), R$Animations$Catch$layers)[[1]]$data
  Trail <- Trail[Trail$Series == 'Past TACs' & as.character(Trail$Frame) == as.character(Proj[7]), ]
  expect_equal(Trail$Year, Proj[1:7])
  expect_equal(Trail$Value, unname(Recorded[as.character(Proj[1:7])]))
  expect_false('Fitted' %in% unlist(lapply(ReplayMP(MSE, 'SurplusProduction', Years = Proj[1:2])$Animations$Catch$layers,
                                           \(L) as.character(L$data$Series))))

  expect_error(ReplayMP(MSE, 'Nope'), 'MP')
  expect_error(ReplayMP(MSE, 1, Years = 1900), 'management years')
})

test_that(".TrimAdviceHistory keeps only earlier advice", {
  A <- Advice(TAC = array(1:3, c(3, 1), list(Year = c('2001', '2002', '2003'), Fleet = 'F1')))
  expect_equal(as.vector(MSEtool:::.TrimAdviceHistory(A, 2003)@TAC), 1:2)
  expect_null(MSEtool:::.TrimAdviceHistory(A, 2001)@TAC)
})

test_that("RecordMP() adds a custom MP's calculations to ReplayMP()", {
  skip_on_cran()
  skip_if_not_installed('gganimate')
  RatioMP <- function(Data, Target = 0.1) {
    Index  <- Data@Survey@Value[, 1]
    Years  <- Data@Years
    Recent <- utils::tail(seq_along(Index), 3)
    Status <- mean(Index[Recent], na.rm = TRUE)
    Mult   <- HockeyStickHCR(1, Est = Status, Ref = Target, ControlPointsIndex = c(0.2, 0.8))
    Trial  <- mean(utils::tail(rowSums(Data@Landings@Value, na.rm = TRUE), 3)) * Mult
    RecordMP(Values = list(Status = Status, Multiplier = Mult), TrialTAC = Trial,
             HCR = RecordHCR(Est = Status, Ref = Target, Value = Mult, ControlPointsIndex = c(0.2, 0.8),
                             xlab = 'Index / target', ylab = 'TAC multiplier'),
             Index = RecordIndex(Index, Years, Name = Data@Survey@Name[1], Status = Status,
                                 StatusYears = Years[Recent], IndexTarget = Target,
                                 ControlPointsIndex = c(0.2, 0.8)))
    Advice(TAC = ConstrainTAC(LastTAC(Data), Trial / LastTAC(Data), c(0.01, 0.2), c(0.01, 0.2), c(0, Inf)))
  }
  class(RatioMP) <- 'mp'

  # no effect outside ReplayMP()
  expect_null(RecordMP(Values = list(a = 1)))
  expect_null(RecordHCR(Est = 1, Value = 1))

  Hist <- Simulate(SingleStockOM, nSim = 1, silent = TRUE)
  MSE  <- Project(Hist, list(RatioMP = RatioMP), silent = TRUE)
  Proj <- Years(MSE@OM, 'Projection')
  R <- expect_no_warning(ReplayMP(MSE, 'RatioMP', Years = Proj[1:4]))
  expect_equal(R$Table$Advice, R$Table$Recorded, tolerance = 1e-8)
  expect_named(R$Table, c('Year', 'AdviceType', 'Advice', 'Recorded', 'Status', 'Multiplier', 'TrialTAC'))
  expect_named(R$Animations, c('Index', 'Status', 'HCR', 'Catch'))
  expect_equal(R$Animations$HCR$labels$x, 'Index / target')
  Pts <- Filter(\(L) all(c('x', 'y') %in% names(L$data)) && nrow(L$data) == 4, R$Animations$HCR$layers)[[1]]$data
  expect_equal(Pts$x, R$Table$Status / 0.1)
  expect_equal(Pts$y, R$Table$Multiplier)

  # invalid records are reported by ReplayMP() as failed replays
  BadMP <- function(Data) {
    RecordMP(HCR = list(x = 1))
    Advice(TAC = LastTAC(Data))
  }
  class(BadMP) <- 'mp'
  MSE2 <- Project(Hist, list(BadMP = BadMP), silent = TRUE)
  expect_error(suppressWarnings(ReplayMP(MSE2, 'BadMP', Years = Proj[1:2])), 'could not be replayed')
})

test_that("MPs stored before they called RecordMP() are recorded through .MPPlot()", {
  Hist <- Simulate(SingleStockOM, nSim = 1, silent = TRUE)
  D <- Hist@Data[[1]][[1]]
  Calc <- MSEtool:::.IndexMPCalc('Rate', D, NULL, 'auto', NULL, NULL, 2, TRUE, 0.3, 1, NULL, 1, 1, NULL,
                                 c(0, 0.5), c(0, 1), 1, c(0.01, 0.5), c(0.01, 0.5), NULL, 'Removals', NULL, 1)
  State <- MSEtool:::.MPReplayState
  State$Capture <- TRUE
  State$Record  <- NULL
  on.exit({ State$Capture <- FALSE; State$Record <- NULL })
  expect_null(MSEtool:::.MPPlot(TRUE, \() .PlotIndexMP(Calc, 'IndexRate', D)))
  expect_s3_class(State$Record$HCR, 'mprecord_hcr')
  expect_s3_class(State$Record$Index, 'mprecord_index')
  expect_equal(State$Record$TrialTAC, Calc$TrialTAC)
})

test_that("replay plots mark the end of the historical period and the MP start year", {
  B <- MSEtool:::.ReplayBoundaries(2026, 2030, 2027:2056)
  expect_equal(unname(B), c(2026.5, 2029.5))
  expect_equal(names(B), c('End of historical period', 'MP start year'))
  expect_length(MSEtool:::.ReplayBoundaries(2026, NULL, 2027:2056), 1)
  expect_length(MSEtool:::.ReplayBoundaries(2026, 2027, 2027:2056), 1)
  L <- MSEtool:::.ReplayBoundaryLayer(B)
  expect_s3_class(L$geom, 'GeomVline')
})
