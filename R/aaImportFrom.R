#' @importFrom abind abind
#' @importFrom dplyr %>%  all_of arrange bind_rows filter group_by left_join mutate pull select summarize
#' summarise
#' @importFrom ggplot2 aes element_blank expand_limits facet_wrap geom_boxplot geom_line
#' ggplot ggplotGrob geom_rect geom_point labs theme theme_bw theme_classic xlim ylim xlab ylab
#' @importFrom graphics abline arrows axis axTicks barplot boxplot contour hist
#' identify layout legend
#'  lines matplot mtext par plot plot.new points polygon segments text title text
#' @importFrom grDevices col2rgb colorRampPalette dev.off jpeg gray png rainbow rgb xy.coords
#' @importFrom grid unit.c unit grid.newpage grid.draw
#' @importFrom methods getClassDef getSlots .hasSlot new show slot slot<- slotNames
#' @importFrom parallel detectCores
#' @importFrom Rcpp evalCpp
#' @importFrom snowfall sfClusterEval sfInit sfExportAll sfIsRunning sfExport
#' sfSapply sfLibrary
#' @importFrom stats acf aggregate approx coef cor dbeta density dnorm dlnorm lm loess
#' loess.smooth nls setNames simulate SSasympOff weighted.mean
#' median nlm nlminb optim optimise optimize plogis pnorm predict qnorm qlnorm quantile rbeta
#' rlnorm rmultinom rnorm runif sd window
#' @importFrom utils  browseURL capture.output combn flush.console packageVersion txtProgressBar setTxtProgressBar
#' ls.str lsf.str read.csv read.csv2
#'
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("MSEtool", libpath)
}


# global variable names
Names <- c("maxage", "R0", "Mexp", "Msd", "dep", "D", "Mgrad", "SRrel", "hs", "procsd",
           "L50", "L95", "L50_95", "CAL_binsmid", "Len_age", "maxlen", "Linf",
           "M_at_Length", "Frac_area_1", "Prob_staying", "M_ageArray", "Mat_age",
           "Wt_age", "V", "Spat_targ", "procmu", "recMulti", "Linfrand", "Krand",
           "Abias Aerr", "Brefbias", "CAA_ESS", "CAA_nsamp", "CAL_ESS", "CAL_bins", "CAL_nsamp",
           "Cbias", "Crefbias", "Csd", "Dbias", "Derr", "TAEFrac", "TAESD", "EffLower",
           "EffUpper", "EffYears", "FMSY_Mbias", "Frac_area_1", "Irefbias", "Isd", "K", "Kbias", "Kgrad",
           "Krand", "Ksd", "L5", "L5s", "LFCbias", "LFS", "LFSbias", "LFSs", "LatASD", "Linfbias", "Linfgrad",
           "Linfrand", "Linfsd", "M", "M_ageArray", "Mat_age", "Mbias", "Mrand", "Prob_staying", "Recsd",
           "SLarray", "SizeLimFrac", "SizeLimSD", "Spat_targ", "TACFrac", "TACSD",
           "Vmaxlen", "Vmaxlens", "Wt_age", "ageM", "betas", "lenMbias", "nCALbins", "procmu", "qcv", "qinc",
           "recMulti",  "t0", "t0bias", "Abias", "Aerr", "Perr", "Esd", "qvar", "Marray",
           "Linfarray", "Karray", "t0array", "mov",  "nareas", "AC", "LenCV", "a", "b", "FinF",
           "Fdisc", "R50", "Rslope", "retA", "retL", "LR5", "LFR", "Rmaxlen",
           "V2", "SLarray2", "DR", "Asize", "Size_area_1", "L50array", "L95array",
           "Fdisc_array", "Fdisc_array2", "Pinitdist", "DataOut",
           'Perr_y', "Cobs", "Iobs", "Dobs", "Btbiascv", 'Btobs', "h", 'Index',
           '.', 'MP', 'Data', 'DataClass', "Type", "Recs", "DominatedMPs",
           'Era', 'SpawnBio', 'Yr', "Sex", 'Beg/Mid', 'steep','int_Age', 'Len_Beg',
           'Wt_Beg', 'Len_Mat', 'Age_Mat', 'Age', "Yr", 'LAA', 'Gender', 'Year',
           'true_Age', 'true_Year', 'true_Yr', 'exp_recr', 'pred_recr', 'Z',
           'season_as_years', 'nseas', 'SR', 'ReqData',
           'Bio_all', 'B', 'C', 'Catch', 'Exp', 'Fleet', 'Model', 'N', 'R', 'SB',
           'SB0', 'Sim', 'age', 'name', 'seas', 'value'
)

if(getRversion() >= "2.15.1") utils::globalVariables(Names)
