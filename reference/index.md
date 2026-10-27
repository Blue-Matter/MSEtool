# Package index

## Operating model

### General functions

- [`OM-class`](https://msetool.openmse.com/reference/OM-class.md) :

  Class `'OM'`

- [`OMdoc()`](https://msetool.openmse.com/reference/OMdoc.md) : Generate
  OM Documentation Report

- [`testOM`](https://msetool.openmse.com/reference/OM-class-objects.md)
  : OM class objects

- [`plot(`*`<pars>`*`)`](https://msetool.openmse.com/reference/plot.Stock.md)
  [`plot(`*`<Stock>`*`)`](https://msetool.openmse.com/reference/plot.Stock.md)
  [`plot(`*`<Fleet>`*`)`](https://msetool.openmse.com/reference/plot.Stock.md)
  [`plot(`*`<Obs>`*`)`](https://msetool.openmse.com/reference/plot.Stock.md)
  [`plot(`*`<Imp>`*`)`](https://msetool.openmse.com/reference/plot.Stock.md)
  [`plot(`*`<Hist>`*`)`](https://msetool.openmse.com/reference/plot.Stock.md)
  [`plot(`*`<OM>`*`)`](https://msetool.openmse.com/reference/plot.Stock.md)
  : Plot Operating Model Object

### Import

Convert stock assessment output, i.e., stock, fleet, and some
observation error parameters, into an operating model

- [`ASAP2OM()`](https://msetool.openmse.com/reference/ASAP2OM.md)
  [`ASAP2Data()`](https://msetool.openmse.com/reference/ASAP2OM.md) :
  Convert ASAP 3 assessments into an operating model

- [`Assess2MOM()`](https://msetool.openmse.com/reference/Assess2MOM.md)
  : Reads bootstrap estimates from a stock assessment model into a
  multi-fleet operating model.

- [`Assess2OM()`](https://msetool.openmse.com/reference/Assess2OM.md)
  [`VPA2OM()`](https://msetool.openmse.com/reference/Assess2OM.md) :

  Reads bootstrap estimates from a stock assessment model (including
  VPA) into an operating model. `Assess2OM` is identical to `VPA2OM`.

- [`Awatea2OM()`](https://msetool.openmse.com/reference/Awatea2OM.md) :
  Reads MCMC estimates from Awatea (Paul Starr) processed r file
  structure into an operating model

- [`BAM2MOM()`](https://msetool.openmse.com/reference/BAM2MOM.md)
  [`BAM2OM()`](https://msetool.openmse.com/reference/BAM2MOM.md) :
  Import a multi-stock, multi-fleet OM from a BAM object

- [`iSCAM2OM()`](https://msetool.openmse.com/reference/iSCAM2OM.md)
  [`iSCAM2Data()`](https://msetool.openmse.com/reference/iSCAM2OM.md) :
  Reads MPD or MCMC estimates and data from iSCAM file structure into an
  operating model

- [`SS2MOM()`](https://msetool.openmse.com/reference/SS2MOM.md)
  [`plot_SS2MOM()`](https://msetool.openmse.com/reference/SS2MOM.md)
  [`SS2OM()`](https://msetool.openmse.com/reference/SS2MOM.md)
  [`SSMOM2OM()`](https://msetool.openmse.com/reference/SS2MOM.md)
  [`plot_SS2OM()`](https://msetool.openmse.com/reference/SS2MOM.md)
  [`MOM_agg_fleets()`](https://msetool.openmse.com/reference/SS2MOM.md)
  : Import Stock Synthesis to MOM (2-sex multi-fleet) or OM (single-sex,
  single-fleet)

- [`WHAM2OM()`](https://msetool.openmse.com/reference/WHAM2OM.md) :
  Takes a fitted SAM model and samples historical population and fishing
  dynamics from the MLE fit and variance-covariance matrix.

### Manual configuration

Manually parameterize the operating model

- [`ChooseEffort()`](https://msetool.openmse.com/reference/Choose.md)
  [`ChooseM()`](https://msetool.openmse.com/reference/Choose.md)
  [`ChooseSelect()`](https://msetool.openmse.com/reference/Choose.md) :
  Manually map parameters for the historical period of operating model
- [`LH2OM()`](https://msetool.openmse.com/reference/LH2OM.md)
  [`predictLH()`](https://msetool.openmse.com/reference/LH2OM.md) :
  Predict missing life-history parameters
- [`ML2D()`](https://msetool.openmse.com/reference/ML2D.md) : Depletion
  and F estimation from mean length of catches
- [`SketchFun()`](https://msetool.openmse.com/reference/SketchFun.md) :
  Manually map the historical relative fishing effort trajectory.
- [`Turing()`](https://msetool.openmse.com/reference/Turing.md)
  [`TuringMOM()`](https://msetool.openmse.com/reference/Turing.md) :
  Turing Test
- [`XL2Fleet()`](https://msetool.openmse.com/reference/XL2Fleet.md) :
  Import Fleet Object from Excel file
- [`XL2OM()`](https://msetool.openmse.com/reference/XL2OM.md) : Load OM
  from Excel file
- [`XL2Stock()`](https://msetool.openmse.com/reference/XL2Stock.md) :
  Import Stock Object from Excel file
- [`makemov()`](https://msetool.openmse.com/reference/makemov.md) :
  Calculates movement matrices from user inputs for fraction in each
  area (fracs) and probability of staying in areas (prob)

## OM components

### Stock

Object specifying biology and stock dynamics

- [`Albacore`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Blue_shark`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Bluefin_tuna`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Bluefin_tuna_WAtl`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Butterfish`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Herring`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Mackerel`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Porgy`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Rockfish`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Snapper`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Sole`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  [`Toothfish`](https://msetool.openmse.com/reference/Stock-class-objects.md)
  : Stock class objects

- [`Stock-class`](https://msetool.openmse.com/reference/Stock-class.md)
  :

  Class `'Stock'`

### Fleet

Object specifying fishing dynamics

- [`DecE_Dom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`DecE_HDom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`DecE_NDom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`FlatE_Dom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`FlatE_HDom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`FlatE_NDom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`Generic_DecE`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`Generic_FlatE`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`Generic_Fleet`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`Generic_IncE`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`IncE_HDom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`IncE_NDom`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`Low_Effort_Non_Target`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`Target_All_Fish`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  [`Targeting_Small_Fish`](https://msetool.openmse.com/reference/Fleet-class-objects.md)
  : Fleet class objects

- [`Fleet-class`](https://msetool.openmse.com/reference/Fleet-class.md)
  :

  Class `'Fleet'`

### Obs

Object specifying observation model (how data are simulated)

- [`Generic_Obs`](https://msetool.openmse.com/reference/Obs-class-objects.md)
  [`Imprecise_Biased`](https://msetool.openmse.com/reference/Obs-class-objects.md)
  [`Imprecise_Unbiased`](https://msetool.openmse.com/reference/Obs-class-objects.md)
  [`Perfect_Info`](https://msetool.openmse.com/reference/Obs-class-objects.md)
  [`Precise_Biased`](https://msetool.openmse.com/reference/Obs-class-objects.md)
  [`Precise_Unbiased`](https://msetool.openmse.com/reference/Obs-class-objects.md)
  : Obs class objects

- [`Obs-class`](https://msetool.openmse.com/reference/Obs-class.md) :

  Class `'Obs'`

### Imp

Object specifying the extent of error in MP implementation

- [`Overages`](https://msetool.openmse.com/reference/Imp-class-objects.md)
  [`Perfect_Imp`](https://msetool.openmse.com/reference/Imp-class-objects.md)
  : Imp class objects

- [`Imp-class`](https://msetool.openmse.com/reference/Imp-class.md) :

  Class `'Imp'`

### Hist

Object generated from the historical reconstruction of the system
dynamics (output of `runMSE(Hist = TRUE)`)

- [`Hist-class`](https://msetool.openmse.com/reference/Hist-class.md) :

  Class `'Hist'`

## Data objects

Includes functions to import and export Data objects, data frames
describing slots, manipulate Data objects, and generate examples.

- [`Atlantic_mackerel`](https://msetool.openmse.com/reference/Data-class-objects.md)
  [`China_rockfish`](https://msetool.openmse.com/reference/Data-class-objects.md)
  [`Cobia`](https://msetool.openmse.com/reference/Data-class-objects.md)
  [`Example_datafile`](https://msetool.openmse.com/reference/Data-class-objects.md)
  [`Gulf_blue_tilefish`](https://msetool.openmse.com/reference/Data-class-objects.md)
  [`ourReefFish`](https://msetool.openmse.com/reference/Data-class-objects.md)
  [`Red_snapper`](https://msetool.openmse.com/reference/Data-class-objects.md)
  [`Simulation_1`](https://msetool.openmse.com/reference/Data-class-objects.md)
  : Data class objects

- [`Data-class`](https://msetool.openmse.com/reference/Data-class.md) :

  Class `'Data'`

- [`Data2csv()`](https://msetool.openmse.com/reference/Data2csv.md) :
  Converts a Data object into a .csv data file

- [`DataInit()`](https://msetool.openmse.com/reference/DataInit.md) :
  Initialize Data Input Files

- [`DataSlots`](https://msetool.openmse.com/reference/DataSlots.md) :
  DataSlots

- [`Data_xl()`](https://msetool.openmse.com/reference/Data_xl.md) : Read
  in Data object from Excel spreadsheet

- [`Lag_Data()`](https://msetool.openmse.com/reference/Lag_Data.md) :

  Lag the time-series slots in a `Data` object by a specified number of
  time-steps

- [`Report()`](https://msetool.openmse.com/reference/Report.md) :
  Generate a Data Report

- [`SS2Data()`](https://msetool.openmse.com/reference/SS2Data.md) :
  Reads data Stock Synthesis file structure into a Data object using
  package r4ss

- [`SS2DataMOM()`](https://msetool.openmse.com/reference/SS2DataMOM.md)
  : Reads data Stock Synthesis file structure into a nested Data object
  analogous with multiMSE

- [`XL2Data()`](https://msetool.openmse.com/reference/XL2Data.md) :
  Import a Data object from Excel file

- [`SimulatedData`](https://msetool.openmse.com/reference/SimulatedData.md)
  : SimulatedData Data

- [`boxplot(`*`<Data>`*`)`](https://msetool.openmse.com/reference/boxplot.Data.md)
  : Boxplot of TAC recommendations

- [`plot(`*`<Data>`*`)`](https://msetool.openmse.com/reference/plot.Data.md)
  : Plot Data object

- [`plotOFL()`](https://msetool.openmse.com/reference/plotOFL.md) : A
  generic OFL plot for NOAA use

- [`summary(`*`<Data>`*`)`](https://msetool.openmse.com/reference/summary-Data.md)
  : Summary of Data object

- [`select_MP()`](https://msetool.openmse.com/reference/select_MP.md) :

  Select DataList for an MP from `MMSE@PPD`

## Management procedures

Calculate advice from management procedures

### General

- [`Can()`](https://msetool.openmse.com/reference/Can.md)
  [`Cant()`](https://msetool.openmse.com/reference/Can.md)
  [`DLMdiag()`](https://msetool.openmse.com/reference/Can.md)
  [`Needed()`](https://msetool.openmse.com/reference/Can.md) : Identify
  management procedures (MPs) based on data availability
- [`Fease()`](https://msetool.openmse.com/reference/Fease.md) : MP
  feasibility diagnostic
- [`Input()`](https://msetool.openmse.com/reference/Input.md) : Function
  to run a set of input control methods
- [`MPtype()`](https://msetool.openmse.com/reference/MPtype.md) :
  Management Procedure Type
- [`RealFease()`](https://msetool.openmse.com/reference/RealFease.md) :
  MP feasibility diagnostic using real data
- [`Required()`](https://msetool.openmse.com/reference/Required.md) :
  What management procedures need what data
- [`Sense()`](https://msetool.openmse.com/reference/Sense.md) :
  Sensitivity analysis
- [`TAC()`](https://msetool.openmse.com/reference/TAC.md) : Calculate
  TAC recommendations for more than one MP
- [`TACfilter()`](https://msetool.openmse.com/reference/TACfilter.md) :
  TAC Filter
- [`tune_MP()`](https://msetool.openmse.com/reference/tune_MP.md) : Tune
  MP
- [`Uses()`](https://msetool.openmse.com/reference/Uses.md) : Find the
  Management Procedures that use a particular data slot
- [`runInMP()`](https://msetool.openmse.com/reference/runInMP.md) : Runs
  input control MPs on a Data object.
- [`runMP()`](https://msetool.openmse.com/reference/runMP.md) : Run a
  Management Procedure

### Reference management procedures

Management procedures reflecting idealized management, e.g., perfect
knowledge of stock productivity, no fishing scenario, etc.

- [`FMSYref()`](https://msetool.openmse.com/reference/FMSYref.md)
  [`FMSYref50()`](https://msetool.openmse.com/reference/FMSYref.md)
  [`FMSYref75()`](https://msetool.openmse.com/reference/FMSYref.md)
  [`NFref()`](https://msetool.openmse.com/reference/FMSYref.md)
  [`curEref()`](https://msetool.openmse.com/reference/FMSYref.md) :
  Reference management procedures

### Other management procedures

- [`Emp()`](https://msetool.openmse.com/reference/Emp.md) : A flexible
  empirical management procedure.

### Rec object

Output of management procedures to implement in the MSE simulation

- [`Rec-class`](https://msetool.openmse.com/reference/Rec-class.md) :

  Class `'Rec'`

## Run MSE simulation

- [`runCOSEWIC()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  [`COSEWIC_Pplot()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  [`COSEWIC_Dplot()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  [`COSEWIC_Blow()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  [`COSEWIC_Hplot()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  [`COSEWIC_report()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  [`COSEWIC_tab()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  [`COSEWIC_tab_formatted()`](https://msetool.openmse.com/reference/runCOSEWIC.md)
  : COSEWIC MSE run using the correct MPs and projected time horizon
- [`Simulate()`](https://msetool.openmse.com/reference/runMSE.md)
  [`Project()`](https://msetool.openmse.com/reference/runMSE.md)
  [`runMSE()`](https://msetool.openmse.com/reference/runMSE.md) : Run a
  Management Strategy Evaluation
- [`setup()`](https://msetool.openmse.com/reference/setup.md) : Setup
  parallel processing

## Evaluate MSE results

### Performance metrics

- [`PMobj-class`](https://msetool.openmse.com/reference/PMobj-class.md)
  : An object for storing data for analysis using data-limited methods
- [`P10()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`P50()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`P100()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`PNOF()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`LTY()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`STY()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`Yield()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`PGK()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`AAVY()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  [`AAVE()`](https://msetool.openmse.com/reference/PerformanceMetric.md)
  : Performance Metrics Methods

### General and diagnostic functions

- [`MSE-class`](https://msetool.openmse.com/reference/MSE-class.md) :

  Class `'MSE'`

- [`Converge()`](https://msetool.openmse.com/reference/Converge.md) :
  Check Convergence

- [`Dom()`](https://msetool.openmse.com/reference/Dom.md) : Determine
  dominate MPs

- [`VOI()`](https://msetool.openmse.com/reference/VOI.md) : Calculate
  Value Of Information

- [`VOI2()`](https://msetool.openmse.com/reference/VOI2.md) : Calculate
  Value Of Information 2

- [`summary(`*`<MSE>`*`)`](https://msetool.openmse.com/reference/summary-MSE.md)
  : Summary of MSE object

### Plot MSE output (general)

- [`Cplot()`](https://msetool.openmse.com/reference/Cplot.md) : Plot the
  median biomass and yield relative to last historical year
- [`Kplot()`](https://msetool.openmse.com/reference/Kplot.md) : KOBE
  plot: a projection by projection plot of F/FMSY and B/BMSY
- [`PMLimit()`](https://msetool.openmse.com/reference/PMLimit.md)
  [`PMObj()`](https://msetool.openmse.com/reference/PMLimit.md) : Create
  a table of Performance Limits and Performance Objectives
- [`Pplot()`](https://msetool.openmse.com/reference/Pplot.md) : A
  projection by projection plot of F/FMSY and B/BMSY
- [`Pplot2()`](https://msetool.openmse.com/reference/Pplot2.md) : A
  projection by projection plot of F/FMSY, B/BMSY, B/B0, and yield
- [`PWhisker()`](https://msetool.openmse.com/reference/PWhisker.md) :
  Performance Whisker Plot
- [`Splot()`](https://msetool.openmse.com/reference/Splot.md) : Standard
  MSE projection plot
- [`SSBrefplot()`](https://msetool.openmse.com/reference/SSBrefplot.md)
  : Plot Spawning stock biomass and reference points for both historical
  and projected period
- [`TradePlot()`](https://msetool.openmse.com/reference/TradePlot.md)
  [`Tplot()`](https://msetool.openmse.com/reference/TradePlot.md)
  [`Tplot2()`](https://msetool.openmse.com/reference/TradePlot.md)
  [`Tplot3()`](https://msetool.openmse.com/reference/TradePlot.md)
  [`NOAA_plot2()`](https://msetool.openmse.com/reference/TradePlot.md) :
  Generic Trade-Plot Function
- [`VOIplot()`](https://msetool.openmse.com/reference/VOIplot.md) : Yet
  another Value of Information Plot
- [`plot(`*`<MSE>`*`)`](https://msetool.openmse.com/reference/plot.MSE.md)
  : Plot MSE object
- [`wormplot()`](https://msetool.openmse.com/reference/wormplot.md) :
  Biomass wormplot

### Plot MSE output (specific to management areas)

- [`DFO_bar()`](https://msetool.openmse.com/reference/DFO_bar.md) :
  Department of Fisheries and Oceans stock status bar plot
- [`DFO_hist()`](https://msetool.openmse.com/reference/DFO_hist.md) :
  Department of Fisheries and Oceans historical plot
- [`DFO_plot()`](https://msetool.openmse.com/reference/DFO_plot.md) :
  Department of Fisheries and Oceans trade-off plot
- [`DFO_plot2()`](https://msetool.openmse.com/reference/DFO_plot2.md) :
  Department of Fisheries and Oceans default plot 2
- [`DFO_quant()`](https://msetool.openmse.com/reference/DFO_quant.md) :
  Department of Fisheries and Oceans biomass quantile plot
- [`DFO_proj()`](https://msetool.openmse.com/reference/DFO_proj.md) :
  Department of Fisheries and Oceans projection plot
- [`DFO_report()`](https://msetool.openmse.com/reference/DFO_report.md)
  : Create a standard DFO MSE report
- [`DFO_spider()`](https://msetool.openmse.com/reference/DFO_spider.md)
  : DFO performance spider plot (top three MPs)
- [`DFO_tab()`](https://msetool.openmse.com/reference/DFO_tab.md) :
  Create a standard DFO performance table
- [`DFO_tab_formatted()`](https://msetool.openmse.com/reference/DFO_tab_formatted.md)
  : A formatted version of the standard DFO performance plot, color
  coded by thresholds
- [`Cos_thresh_tab()`](https://msetool.openmse.com/reference/Cos_thresh_tab.md)
  : Current default thresholds for COSEWIC satisficing
- [`Thresh_tab()`](https://msetool.openmse.com/reference/Thresh_tab.md)
  : Current default thresholds for DFO satisficing
- [`NOAA_plot()`](https://msetool.openmse.com/reference/NOAA_plot.md) :
  National Oceanographic and Atmospheric Administration default plot 1

## multiMSE

Multi-population (two-sex or multi-stock), multi-fleet operating models
(class MOM) and corresponding MSE object (class MMSE)

- [`Albacore_TwoFleet`](https://msetool.openmse.com/reference/MOM-class-objects.md)
  : MOM class objects

- [`MOM-class`](https://msetool.openmse.com/reference/MOM-class.md) :

  Class `'MOM'`

- [`MMSE-class`](https://msetool.openmse.com/reference/MMSE-class.md) :

  Class `'MMSE'`

- [`makeMOM()`](https://msetool.openmse.com/reference/makeMOM.md) :
  Utility for making multi-OMs

- [`makeRel()`](https://msetool.openmse.com/reference/makeRel.md)
  [`print(`*`<Rel>`*`)`](https://msetool.openmse.com/reference/makeRel.md)
  [`predict(`*`<Rel>`*`)`](https://msetool.openmse.com/reference/makeRel.md)
  [`simulate(`*`<Rel>`*`)`](https://msetool.openmse.com/reference/makeRel.md)
  : MICE relationships for multi-OM

- [`SimulateMOM()`](https://msetool.openmse.com/reference/multiMSE.md)
  [`ProjectMOM()`](https://msetool.openmse.com/reference/multiMSE.md)
  [`multiMSE()`](https://msetool.openmse.com/reference/multiMSE.md) :
  Run a multi-fleet multi-stock Management Strategy Evaluation

- [`multidebug()`](https://msetool.openmse.com/reference/multidebug.md)
  : A basic comparison of runMSE output (MSE) and multiMSE (MMSE)

- [`plot(`*`<MMSE>`*`)`](https://msetool.openmse.com/reference/plot.MMSE.md)
  : Standard plot for an object of class MMSE (multi MSE)

- [`plot(`*`<MOM>`*`,`*`<missing>`*`)`](https://msetool.openmse.com/reference/plot.MOM.md)
  : Standard plot for an object of class MOM

- [`plotRel()`](https://msetool.openmse.com/reference/plotRel.md) : Plot
  a relationship between stocks

- [`plotmulti()`](https://msetool.openmse.com/reference/plotmulti.md) :
  A basic SSB plot for debugging runMSE output

- [`summary(`*`<MMSE>`*`)`](https://msetool.openmse.com/reference/summary-MMSE.md)
  : Summary of MMSE object

## Miscellaneous

Convenient user functions to process various objects

- [`avail()`](https://msetool.openmse.com/reference/avail.md) : What
  objects of this class are available

- [`optCPU()`](https://msetool.openmse.com/reference/optCPU.md) :
  Determine optimal number of cpus

- [`tinyErr()`](https://msetool.openmse.com/reference/tinyErr.md) :
  Remove observation, implementation, and process error

- [`writeCSV()`](https://msetool.openmse.com/reference/writeCSV.md) :
  Internal function to write CSVs for objects

- [`print(`*`<multiHist>`*`)`](https://msetool.openmse.com/reference/print.multiHist.md)
  :

  Print method for `multiHist` objects

### OM

- [`CheckOM()`](https://msetool.openmse.com/reference/CheckOM.md) :
  Check OM object is complete
- [`Replace()`](https://msetool.openmse.com/reference/Replace.md) :
  Replace an existing Stock, Fleet, Obs, or Imp object
- [`MSEextra()`](https://msetool.openmse.com/reference/MSEextra.md) :
  Load more data from MSEextra package
- [`SubCpars()`](https://msetool.openmse.com/reference/SubCpars.md) :
  Subset the cpars slot in an operating model
- [`SubOM()`](https://msetool.openmse.com/reference/SubOM.md) : Subset a
  Stock, Fleet, Obs, or Imp object from an OM object
- [`validcpars()`](https://msetool.openmse.com/reference/validcpars.md)
  : Valid custom parameters (cpars)

### Data

- [`CALsimp()`](https://msetool.openmse.com/reference/CALsimp.md) :
  Simplifies the CAL slot of data object
- [`CheckMPs()`](https://msetool.openmse.com/reference/CheckMPs.md) :
  Check that specified MPs are valid and will run on
  MSEtool::SimulatedData
- [`replic8()`](https://msetool.openmse.com/reference/replic8.md) :
  Enlarge (replicate) a DLM data object to create an additional
  dimension for simulation / sensitivity testing

### MP

- [`MPtype()`](https://msetool.openmse.com/reference/MPtype.md) :
  Management Procedure Type
- [`CombineMMP()`](https://msetool.openmse.com/reference/CombineMMP.md)
  : Create a blank MP recommendations object (class Rec) of the right
  dimensions
- [`doHCR()`](https://msetool.openmse.com/reference/doHCR.md) : Hockey
  Stick Harvest control rule that modifies TAC.
- [`doIfreq()`](https://msetool.openmse.com/reference/doIfreq.md) :
  Create indices that are sampled at various frequencies
- [`doRec()`](https://msetool.openmse.com/reference/doRec.md) :
  Calculate a management recommendation given constraints
- [`smoothy()`](https://msetool.openmse.com/reference/smoothy.md) :
  General purpose polynomial smoother

### MSE

- [`Sub()`](https://msetool.openmse.com/reference/Sub.md) : Subset MSE
  object by management procedure (MP) or simulation.
- [`checkMSE()`](https://msetool.openmse.com/reference/checkMSE.md)
  [`addMPs()`](https://msetool.openmse.com/reference/checkMSE.md)
  [`joinMSE()`](https://msetool.openmse.com/reference/checkMSE.md)
  [`joinHist()`](https://msetool.openmse.com/reference/checkMSE.md)
  [`updateMSE()`](https://msetool.openmse.com/reference/checkMSE.md) :
  Utility functions for MSE objects
- [`joinData()`](https://msetool.openmse.com/reference/joinData.md) :
  Join Data objects present in a list

### MMSE

- [`addMMPs()`](https://msetool.openmse.com/reference/addMMPs.md) :
  Utility functions for multiMSE objects

### Internal functions

Populations dynamics functions and other processing functions

- [`MSYCalcs()`](https://msetool.openmse.com/reference/MSYCalcs.md) :
  Internal function to calculate MSY Reference Points

- [`applyMP()`](https://msetool.openmse.com/reference/applyMP.md) :
  Apply Management Procedures to an object of class Data

- [`applyMMP()`](https://msetool.openmse.com/reference/applyMMP.md) :
  Apply multi Management Procedures (class MMP) to a hierarchical list
  of Data class objects

- [`calcRefYield()`](https://msetool.openmse.com/reference/calcRefYield.md)
  : Calculate Reference Yield

- [`cparscheck()`](https://msetool.openmse.com/reference/cparscheck.md)
  :

  Internal function for checking that the `OM@cpars` is formatted
  correctly

- [`dnormal()`](https://msetool.openmse.com/reference/dnormal.md) :
  Double-normal selectivity curve

- [`getmov2()`](https://msetool.openmse.com/reference/getmov2.md) :
  Optimization function to find a movement model that matches user
  specified movement characteristics modified for Rcpp.

- [`getsel()`](https://msetool.openmse.com/reference/getsel.md) :
  Calculate selectivity curve

- [`hconv()`](https://msetool.openmse.com/reference/hconv.md)
  [`R0conv()`](https://msetool.openmse.com/reference/hconv.md)
  [`SRalphaconv()`](https://msetool.openmse.com/reference/hconv.md)
  [`SRbetaconv()`](https://msetool.openmse.com/reference/hconv.md) :
  Stock recruit parameterization

- [`movestockCPP()`](https://msetool.openmse.com/reference/movestockCPP.md)
  : Apply the movement model to the stock for one time-step

- [`simCAA()`](https://msetool.openmse.com/reference/simCAA.md) :
  Simulate Catch-at-Age Data

- [`simCAL()`](https://msetool.openmse.com/reference/simCAL.md) :
  Simulate Catch-at-Length Data

- [`simmov()`](https://msetool.openmse.com/reference/simmov.md)
  [`plot_mov()`](https://msetool.openmse.com/reference/simmov.md) :
  Calculates movement matrices from user inputs

### Internal multiMSE

- [`expandHerm()`](https://msetool.openmse.com/reference/Herm-int.md)
  [`checkHerm()`](https://msetool.openmse.com/reference/Herm-int.md)
  [`subsetHerm()`](https://msetool.openmse.com/reference/Herm-int.md) :
  Internal Herm functions
- [`multiData()`](https://msetool.openmse.com/reference/multiData.md) :
  Combine data among fleets
- [`multiDataS()`](https://msetool.openmse.com/reference/multiDataS.md)
  : Combine data among stocks

### Figures

- [`join_plots()`](https://msetool.openmse.com/reference/join_plots.md)
  : Plot several plots with a shared legend
- [`makeTransparent()`](https://msetool.openmse.com/reference/makeTransparent.md)
  : Make colors transparent
- [`plotFun()`](https://msetool.openmse.com/reference/plotFun.md) :
  Print out plotting functions
- [`plotquant()`](https://msetool.openmse.com/reference/plotquant.md) :
  A fairly tidy time-series quantile plot
- [`quantile_plot()`](https://msetool.openmse.com/reference/quantile_plot.md)
  : A quantile plot
