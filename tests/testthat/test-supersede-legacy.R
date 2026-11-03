legacy_superseded <- c(
  "SS2OM", "SS2MOM", "SS2Data", "SS2DataMOM", "SSMOM2OM", "MOM_agg_fleets",
  "plot_SS2OM", "plot_SS2MOM", "BAM2MOM", "WHAM2OM", "iSCAM2OM",
  "iSCAM2Data", "iSCAMcomps", "iSCAMinds", "ASAP2OM", "ASAP2Data",
  "Awatea2OM", "VPA2OM", "Assess2OM", "Assess2MOM", "OMinit", "OMdoc",
  "OMexample", "Data_xl", "XL2OM", "XL2Data", "XL2Stock", "XL2Fleet",
  "Data2csv", "CALsimp", "DataInit", "DFO_bar", "DFO_hist", "DFO_plot",
  "DFO_plot2", "DFO_proj", "DFO_quant", "DFO_report", "DFO_spider",
  "DFO_tab", "DFO_tab_formatted", "COSEWIC_Blow", "COSEWIC_Dplot",
  "COSEWIC_Hplot", "COSEWIC_Pplot", "COSEWIC_report", "COSEWIC_tab",
  "COSEWIC_tab_formatted", "runCOSEWIC", "Thresh_tab", "Cos_thresh_tab",
  "plotquant", "NOAA_plot", "NOAA_plot2", "Turing", "TuringMOM", "Fease",
  "RealFease", "PMLimit", "PMObj", "Kplot", "Pplot", "Pplot2", "Tplot",
  "Tplot2", "Tplot3", "TradePlot", "Cplot", "PWhisker", "SSBrefplot",
  "wormplot", "join_plots", "Splot", "quantile_plot", "VOI", "VOI2",
  "VOIplot", "plotmulti", "multidebug", "plotRel", "plotOFL", "hist2",
  "plotFun", "SketchFun", "ChooseM", "ChooseSelect", "ChooseEffort",
  "LH2OM", "predictLH", "tune_MP", "Emp", "doHCR", "doIfreq", "doRec",
  "smoothy", "DLMdiag", "Sense", "OneRep", "replic8", "CSRA", "ML2D",
  "MPurl", "select_MP", "Converge", "makemov", "makemov2", "simmov",
  "simmov2", "plot_mov", "CalcAsymptoticDist", "makeMOM", "makeRel",
  "hconv", "R0conv", "SRalphaconv", "SRbetaconv", "tdlnorm"
)

test_that("superseded legacy functions call .SupersedeLegacy() first", {
  ns <- asNamespace("MSEtool")
  for (fn in legacy_superseded) {
    b <- body(get(fn, envir = ns))
    first <- if (is.call(b) && identical(b[[1]], as.name("{"))) b[[2]] else b
    expect_true(is.call(first) && identical(first[[1]], as.name(".SupersedeLegacy")), info = fn)
    expect_identical(first[[2]], paste0(fn, "()"), info = fn)
  }
})

test_that("superseded legacy functions do not warn", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_no_warning(x <- tdlnorm(1, 1, 0.1))
  expect_equal(x, dlnorm(1, mconv(1, 0.1), sdconv(1, 0.1)))
  expect_no_warning(hconv(5, 10))
})
