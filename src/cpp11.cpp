// Generated by cpp11: do not edit by hand
// clang-format off

#include <cpp11/R.hpp>
#include <Rcpp.h>
using namespace Rcpp;
#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// code.cpp
void fun();
extern "C" SEXP _MSEtool_fun() {
  BEGIN_CPP11
    fun();
    return R_NilValue;
  END_CPP11
}

extern "C" {
/* .Call calls */
extern SEXP _MSEtool_CalcBiomass_(SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_CalcCatch_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_CalcDensity_(SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_CalcFArea_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_CalcFfromCatch_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_CalcVBiomass_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_DistEffort_(SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_LinInterp_cpp(SEXP, SEXP, SEXP);
extern SEXP _MSEtool_MSYCalcs(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_Ref_int_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_calcVatAge(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_combine(SEXP);
extern SEXP _MSEtool_genSizeComp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_genSizeComp2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_get_freq(SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_get_freq2(SEXP, SEXP, SEXP);
extern SEXP _MSEtool_grav(SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_movestockCPP(SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_movfit_Rcpp(SEXP, SEXP, SEXP);
extern SEXP _MSEtool_popdynCPP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_popdynOneTScpp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _MSEtool_rnormSelect2(SEXP, SEXP, SEXP);
extern SEXP _MSEtool_tdnorm(SEXP, SEXP, SEXP);
extern SEXP _MSEtool_vecminInd(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_MSEtool_CalcBiomass_",    (DL_FUNC) &_MSEtool_CalcBiomass_,     4},
    {"_MSEtool_CalcCatch_",      (DL_FUNC) &_MSEtool_CalcCatch_,      12},
    {"_MSEtool_CalcDensity_",    (DL_FUNC) &_MSEtool_CalcDensity_,     4},
    {"_MSEtool_CalcFArea_",      (DL_FUNC) &_MSEtool_CalcFArea_,       9},
    {"_MSEtool_CalcFfromCatch_", (DL_FUNC) &_MSEtool_CalcFfromCatch_, 11},
    {"_MSEtool_CalcVBiomass_",   (DL_FUNC) &_MSEtool_CalcVBiomass_,    6},
    {"_MSEtool_DistEffort_",     (DL_FUNC) &_MSEtool_DistEffort_,      4},
    {"_MSEtool_LinInterp_cpp",   (DL_FUNC) &_MSEtool_LinInterp_cpp,    3},
    {"_MSEtool_MSYCalcs",        (DL_FUNC) &_MSEtool_MSYCalcs,        17},
    {"_MSEtool_Ref_int_cpp",     (DL_FUNC) &_MSEtool_Ref_int_cpp,     12},
    {"_MSEtool_calcVatAge",      (DL_FUNC) &_MSEtool_calcVatAge,       7},
    {"_MSEtool_combine",         (DL_FUNC) &_MSEtool_combine,          1},
    {"_MSEtool_fun",             (DL_FUNC) &_MSEtool_fun,              0},
    {"_MSEtool_genSizeComp",     (DL_FUNC) &_MSEtool_genSizeComp,     11},
    {"_MSEtool_genSizeComp2",    (DL_FUNC) &_MSEtool_genSizeComp2,    11},
    {"_MSEtool_get_freq",        (DL_FUNC) &_MSEtool_get_freq,         4},
    {"_MSEtool_get_freq2",       (DL_FUNC) &_MSEtool_get_freq2,        3},
    {"_MSEtool_grav",            (DL_FUNC) &_MSEtool_grav,             4},
    {"_MSEtool_movestockCPP",    (DL_FUNC) &_MSEtool_movestockCPP,     4},
    {"_MSEtool_movfit_Rcpp",     (DL_FUNC) &_MSEtool_movfit_Rcpp,      3},
    {"_MSEtool_popdynCPP",       (DL_FUNC) &_MSEtool_popdynCPP,       31},
    {"_MSEtool_popdynOneTScpp",  (DL_FUNC) &_MSEtool_popdynOneTScpp,   5},
    {"_MSEtool_rnormSelect2",    (DL_FUNC) &_MSEtool_rnormSelect2,     3},
    {"_MSEtool_tdnorm",          (DL_FUNC) &_MSEtool_tdnorm,           3},
    {"_MSEtool_vecminInd",       (DL_FUNC) &_MSEtool_vecminInd,        1},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_MSEtool(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
