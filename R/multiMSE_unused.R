# multiMSE <- function(MOM, MPs = list(c("AvC","DCAC"),c("FMSYref","curE")),
#                      CheckMPs = FALSE, timelimit = 1, Hist=FALSE, ntrials=50, fracD=0.05, CalcBlow=FALSE,
#                      HZN=2, Bfrac=0.5, AnnualMSY=TRUE, silent=FALSE, PPD=FALSE, parallel=FALSE,
#                      save_name=NULL, checks=FALSE, control=NULL) {
#
#
#   if (class(MOM)!='MOM') stop("OM object is not of class '<OM'", call. = FALSE)
#
#   # Check MPs
#   MPvec<-unlist(MPs)
#   if (!all(is.na(MPvec))) {
#     for (mm in MPvec) {
#       chkMP <- try(get(mm), silent=TRUE)
#       if (!(class(chkMP) %in% c('MP','MMP'))) stop(mm, " is not a valid MP", call.=FALSE)
#     }
#   }
#
#   MSE1 <- multiMSE_int(MOM, MPs, CheckMPs, timelimit, Hist, ntrials, fracD, CalcBlow,
#                        HZN, Bfrac, AnnualMSY, silent, PPD, checks=checks, control=control)
#
#   MSE1
# }
