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
  Lines <- R$Animations$Status$layers[[2]]$data
  expect_true(all(as.numeric(Lines$Year) <= as.numeric(as.character(Lines$Frame))))
  OMS <- MSEtool:::.ReplayOMStatus(MSE, 'IndexRate', 2, R$Stock, 'SB')
  K <- attr(OMS, 'K')
  expect_true(K > 0 && K < 1)
  MSY <- MSEtool:::.ReplayOMRelative(MSE, 'IndexRate', 2, R$Stock, 'SB_SBMSY')
  Both <- merge(OMS, MSY, by = 'Year')
  expect_equal(Both$Value.x, Both$Value.y * K, tolerance = 0.05)
  OMI <- R$Animations$Index$layers[[2]]$data
  expect_true(all(OMI$Year < as.numeric(as.character(OMI$Frame))))
  CatchSeries <- unique(as.character(R$Animations$Catch$layers[[1]]$data$Series))
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
  First <- \(a) min(a$layers[[2]]$data$Year)
  expect_gt(First(Cut), First(Full))
  expect_equal(min(diff(sort(unique(Cut$layers[[2]]$data$Year)))), 1)
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
